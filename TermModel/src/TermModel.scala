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

import ch.ethz.dal.tinyir.processing.Tokenizer

object TermModel {

  def loadTrainingQueries(queriesPath: String, model: TopicModel, topics: TipsterTopicParser, topicMapping: Map[Int, Int]): collection.immutable.Map[Int, Query] = {
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
          //Mapping topics
          val topicId = model.topics(TermFrequencies.tf(Tokenizer.getTokens(line))).argmax
          if (topicMapping.contains(topicId)) {
            queriesTexts += new Query(line + " " + topics.getVocabularySummary(topicMapping(topicId)))
          } else {
            queriesTexts += new Query(line)
          }
        }
      }
    }
    return (queriesIds zip queriesTexts).toMap[Int, Query]
  }

  def loadTrainingQueries(queriesPath: String): collection.immutable.Map[Int, Query] = {
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
          queriesTexts += new Query(line)
        }
      }
    }
    return (queriesIds zip queriesTexts).toMap[Int, Query]
  }

  def loadTestQueries(testTopics: TipsterTopicParser, model: TopicModel, topics: TipsterTopicParser, topicMapping: Map[Int, Int]): collection.immutable.Map[Int, Query] = {
    var queriesIds = new MutableList[Int]()
    var queriesTexts = new MutableList[Query]()

    for (topic <- testTopics.topics) {
      println("Topic id " + topic.t_num + " content: " + topic.t_title)
      queriesIds += topic.t_num
      val topicId = model.topics(TermFrequencies.tf(Tokenizer.getTokens(topic.t_title))).argmax
      if (topicMapping.contains(topicId)) {
        queriesTexts += new Query(topic.t_title + " " + topics.getVocabularySummary(topicMapping(topicId)))
      } else {
        queriesTexts += new Query(topic.t_title)
      }
    }
    return (queriesIds zip queriesTexts).toMap[Int, Query]
  }

  def loadTestQueries(testTopics: TipsterTopicParser): collection.immutable.Map[Int, Query] = {
    var queriesIds = new MutableList[Int]()
    var queriesTexts = new MutableList[Query]()

    for (topic <- testTopics.topics) {
      println("Topic id " + topic.t_num + " content: " + topic.t_title)
      queriesIds += topic.t_num
      queriesTexts += new Query(topic.t_title)
    }
    return (queriesIds zip queriesTexts).toMap[Int, Query]
  }

  def loadGroundTruth(groundTruthPath: String): TipsterGroundTruth = {
    val t = new TipsterGroundTruth(groundTruthPath)
    t.judgements.foreach(j => println("Topic " + j._1 + ": " + j._2.size + " judgements found."))
    return t
  }

  def main(args: Array[String]) {
    val queryDoc = scala.io.Source.fromFile("src/resources/queries")
    val queryIds = new ListBuffer[Int]()
    val queries = new ListBuffer[String]()
    val synonymDoc = scala.io.Source.fromFile("src/resources/synonyms")

    var cnt = 0
    var synonyms = new HashMap[Int, List[String]]()
    var synonymGroup = new HashMap[String, Int]()
    for (s <- synonymDoc.getLines()) {
      cnt += 1
      val words = s.split(" +").map(_.trim)
      synonyms += cnt -> words.toList

      for (w <- words) {
        synonymGroup += w -> cnt
      }
    }

    val trainingTopicsPath = "src/resources/topics"
    //Topic modeling
    val topics = new TipsterTopicParser(trainingTopicsPath)
    val topicsFinal = new TipsterTopicParser(trainingTopicsPath)
    topics.parse()

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

    //println(topicMapping)
    /*var topicVocs = Map[Int, Set[String]]().withDefaultValue(Set.empty)
    for ((w, a) <- model.Pwt) {
      for (id <- 0 until ntopics) {
        //println((a.arr).length + " " +  a.arr(10))
        if (a.arr(id) > 0) {
          topicVocs(id).add(w)
        }
      }
    }
    
    
    println(topicMapping.mkString("\n"))*/

    //model.Pwt.foreach{ case (w,a) => println(w + ": " + a.mkString(" ")) } 

    val queriesPath = "src/resources/queries"
    val trainingQueriesPath = "src/resources/IR2015/tipster/qrels"
    var queries2 = loadTrainingQueries(queriesPath, model, topics, topicMapping)
    var testQueries = loadTestQueries(topicsFinal, model, topics, topicMapping)

    println(queries)
    var groundTruth = loadGroundTruth(trainingQueriesPath)

    /*for (q <- queryDoc.getLines()) {
      try {
        val id = q.toInt
        queryIds += id
      } catch {
        case e: Exception => { 
          //Mapping topics
          val topicId = model.topics(TermFrequencies.tf(Tokenizer.getTokens(q))).argmax
          if (topicMapping.contains(topicId)) {
            queries += q + " " + topics.getVocabularySummary(topicMapping(topicId))
          }
          else {
            queries += q 
          }
        }
      }
    }*/

    println("Queries " + queries2.mkString("\n"))

    val alerts = new AlertsCosine(queries2, 100)

    var iter = new TipsterCorpusIterator("src/resources/IR2015/tipster/zips")

    var i = 1
    while (iter.hasNext) {
      val doc = iter.next
      alerts.preProcess(doc.body)

      i += 1
      if (i % 10000 == 0) {
        println(i)
      }
    }

    println("Preprocessing done")

    iter = new TipsterCorpusIterator("src/resources/IR2015/tipster/zips")
    i = 1

    while (iter.hasNext) {
      val doc = iter.next
      alerts.process(doc.name, doc.body)

      i += 1
      if (i % 100 == 0) {
        println(i)
      }
    }

    val pw = new PrintWriter("./resultsCosine")
    var totalF1: Double = 0
    for (q <- queries2) {
      pw.write(q._1 + "\n")
      val rel = groundTruth.judgements.get(q._1 + "").get.toSet
      pw.write(rel.toString() + "\n")
      val ret_term = alerts.results(q._1)
      val t = ret_term.map(_.title).toSet
      pw.write(t.toString() + "\n")

      val truePos = (t & rel).size
      val precision = truePos.toDouble / t.size.toDouble
      val recall = truePos.toDouble / Math.min(rel.size.toDouble, 100.0)

      pw.write(precision + "\n")
      pw.write(recall + "\n")
      if (precision != 0 && recall != 0) {
        totalF1 += precision * recall * 2 / (precision + recall)
      }
    }

    pw.write(totalF1 / queries2.size + "")
    pw.flush()
    pw.close()
  }
}