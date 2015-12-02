import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Paths

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.MutableList
import scala.collection.mutable.Set

import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import ch.ethz.dal.tinyir.lectures.TopicModel
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import ch.ethz.dal.tinyir.lectures.PrecisionRecall

import ch.ethz.dal.tinyir.processing.Tokenizer

class TermModel(st: String, nbDoc: Int) {
  val nbDocReturned = nbDoc
  val scoreType = st
  val trainingTopicsPath = "src/resources/topics"
  val testTopicsPath = "src/resources/topics-final"
  
  var tokenizer = new Tokenizer()

  val topics = new TipsterTopicParser(trainingTopicsPath, tokenizer)
  val topicsFinal = new TipsterTopicParser(testTopicsPath, tokenizer)
  topics.parse()
  topicsFinal.parse()

  val queriesPath = "src/resources/queries"
  val trainingQueriesPath = "src/resources/IR2015/tipster/qrels"

  //Topic modeling
  val vocabulary = topics.topics.map(_.qterms.toSet).reduce(_ | _)
  val ntopics = topics.topics.size
  val model = new TopicModel(vocabulary, ntopics)
  val stream = topics.topics.map { case x => TermFrequencies.tf(x.qterms) }.toStream

  //Learning iterations
  for (i <- 0 until 100) model.learn(stream)

  //Mapping the max output in  modeler to original topic number as defined in topics file
  var topicMapping = Map[Int, Int]()

  //model.Pwt.foreach{ case (w,a) => println(w + ": " + a.mkString(" ")) } 
  for (i <- 0 until ntopics) {
    println("*** Topic model for doc " + i + " = " + model.topics(stream(i)).mkString(" ") + " ***")
    //Mapping topic
    println(i + " is topic " + topics.topics(i).t_num + " mapped to " + model.topics(stream(i)).argmax)
    //println(model.topics(stream(i)).argmax)
    topicMapping(model.topics(stream(i)).argmax) = topics.topics(i).t_num
  }

  var trainingQueries = loadTrainingQueries(true)
  var testQueries = loadTestQueries(true)

  var groundTruth = loadGroundTruth()

  var alerts: Alerts = null
  if (st == "cosine") {
    alerts = new AlertsCosine(testQueries, nbDocReturned, tokenizer)
  } else if (st == "mle") {
    alerts = new AlertsMLE(testQueries, nbDocReturned, tokenizer)
  } else if (st == "tf") {
    alerts = new AlertsTF(testQueries, nbDocReturned, tokenizer)
  } else if (st == "idf") {
    alerts = new AlertsIDF(testQueries, nbDocReturned, tokenizer)
  } else if (st == "tfidf") {
    alerts = new AlertsTfIdf(testQueries, nbDocReturned, tokenizer)
  }

  private def loadTrainingQueries(isTopicModelling: Boolean): collection.immutable.Map[Int, Query] = {
    var queriesIds = new MutableList[Int]()
    var queriesTexts = new MutableList[Query]()
    if (Files.exists(Paths.get(queriesPath))) {
      println("Found queries at " + queriesPath + "\n")

      val queryDoc = scala.io.Source.fromFile(queriesPath)
      for (line <- queryDoc.getLines()) {
        println("Line " + line)

        val numeric_regex = """^\s*\d+\s*$""".r

        if (line.matches("""^\s*\d+\s*$""")) {
          queriesIds += line.toInt
        } else {
          if (isTopicModelling) {
            //Mapping topics
            val topicId = model.topics(TermFrequencies.tf(tokenizer.getTokens(line))).argmax
            if (topicMapping.contains(topicId)) {
              queriesTexts += new Query(line + " " + topics.getVocabularySummary(topicMapping(topicId)))
            } else {
              queriesTexts += new Query(line)
            }
          } else {
            queriesTexts += new Query(line)
          }
        }
      }
    }
    return (queriesIds zip queriesTexts).toMap[Int, Query]
  }

  private def loadTestQueries(isTopicModelling: Boolean): collection.immutable.Map[Int, Query] = {
    var queriesIds = new MutableList[Int]()
    var queriesTexts = new MutableList[Query]()

    for (topic <- topicsFinal.topics) {
      println("Topic id " + topic.t_num + " content: " + topic.t_title)
      queriesIds += topic.t_num
      if (isTopicModelling) {
        //Mapping topics
        val topicId = model.topics(TermFrequencies.tf(tokenizer.getTokens(topic.t_title))).argmax
        if (topicMapping.contains(topicId)) {
          queriesTexts += new Query(topic.t_title + " " + topics.getVocabularySummary(topicMapping(topicId)))
        } else {
          queriesTexts += new Query(topic.t_title)
        }
      } else {
        queriesTexts += new Query(topic.t_title)
      }
    }
    return (queriesIds zip queriesTexts).toMap[Int, Query]
  }

  def loadGroundTruth(): TipsterGroundTruth = {
    val t = new TipsterGroundTruth(trainingQueriesPath)
    t.judgements.foreach(j => println("Topic " + j._1 + ": " + j._2.size + " judgements found."))
    return t
  }

  def testModel() {
    if (alerts != null) {
      val zipPath = "src/resources/IR2015/tipster/zips"
      var iter = new TipsterCorpusIterator(zipPath)
      var i = 0

      if (st != "tf") {
        while (iter.hasNext) {
          val doc = iter.next
          alerts.preProcess(doc.body)

          i += 1
          if (i % 10000 == 0) {
            println(i)
          }
        }
        println("Preprocessing done")
      }

      if (st != "idf") {
        iter = new TipsterCorpusIterator("src/resources/IR2015/tipster/zips")
        i = 0

        while (iter.hasNext) {
          val doc = iter.next
          alerts.process(doc.name, doc.body)

          i += 1
          if (i % 100 == 0) {
            println(i)
          }
        }
      }
      println("Processing done")
    } else {
      println("Please enter the scoring you want to evaluate:\n"
        + "cosine\n"
        + "mle\n"
        + "tf\n"
        + "idf\n")
    }
  }

  def evaluateModel() {
    if (alerts != null) {
      val pw = new PrintWriter("./results-" + st)
      //Results: use Logger.append to write to file
      val resPath = "ranking-" + st + "-2.run"
      val resLogger = new ResultLogger(resPath)
      val statResPath = "score-2.txt"
      val statLogger = new ResultLogger(statResPath)

      //Output format: topic-id rank document-id

      var totalF1: Double = 0
      var map_score: Double = 0.0
      var pres_score: Double = 0.0
      var rec_score: Double = 0.0
      var f1_score: Double = 0.0
      var train_map_score: Double = 0.0
      var train_pres_score: Double = 0.0
      var train_rec_score: Double = 0.0
      var train_f1_score: Double = 0.0

      for (q <- testQueries) {
        val rel = groundTruth.judgements.get(q._1 + "").get.toSet
        val ret = alerts.results(q._1).map(r => r.title)

        println("Results for query (language model) " + q._1 + " " + q._2.origQuery)

        val pr = new PrecisionRecall(ret, rel, nbDoc)
        val score = PrecisionRecall.evaluate(ret.toSet, rel, nbDoc).score
        pres_score += score._1
        rec_score += score._2
        f1_score += score._3
        
        

        statLogger.append("Query " + 1)
        statLogger.append("Precision: " + pres_score)
        statLogger.append("Recall: " + rec_score)
        statLogger.append("F1: " + f1_score)

        resLogger.append(ret.zipWithIndex.map { case (t, i) => q._1 + " " + 1 + i + " " + t }.mkString("\n"))

        println(pr.relevIdx.mkString(" "))
        println(pr.precs.mkString(" "))
        println(pr.aps)
        println(pr.iprecs.mkString(" "))
        println(pr.iaps)
        statLogger.append("AP: " + pr.aps)
        map_score += pr.aps
        
        if (trainingQueries.keySet.contains(q._1)) {
          train_pres_score += score._1
          train_rec_score += score._2
          train_f1_score += score._3
          train_map_score += pr.aps
        }
      }

      statLogger.append("******************* System *******************")
      train_map_score /= trainingQueries.size
      train_pres_score /= trainingQueries.size
      train_rec_score /= trainingQueries.size
      train_f1_score /= trainingQueries.size
      map_score /= testQueries.size
      pres_score /= testQueries.size
      rec_score /= testQueries.size
      f1_score /= testQueries.size
      statLogger.append("Training Precision: " + train_pres_score)
      statLogger.append("Training Recall: " + train_rec_score)
      statLogger.append("Training F1: " + train_f1_score)
      statLogger.append("Training MAP: " + train_map_score)
      statLogger.append("Precision: " + pres_score)
      statLogger.append("Recall: " + rec_score)
      statLogger.append("F1: " + f1_score)
      statLogger.append("MAP: " + map_score)

    } else {
      println("Please enter the scoring you want to evaluate:\n"
        + "cosine\n"
        + "mle\n"
        + "tf\n"
        + "idf\n")
    }
  }

}

object TermModel {

  def main(args: Array[String]) {

    if (args.length == 1) {
      var model = new TermModel(args(0), 100)
      model.testModel()
      model.evaluateModel()
    } else {
      println("Please enter the scoring you want to evaluate:\n"
        + "cosine\n"
        + "mle\n"
        + "tf\n"
        + "idf\n")
    }
  }
}