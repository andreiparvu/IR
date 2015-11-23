package main.scala.termmodel

import scala.collection.mutable.MutableList
import scala.collection.mutable.Map
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.processing._
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import ch.ethz.dal.tinyir.indexing.FreqIndex
import ch.ethz.dal.tinyir.alerts.AlertsTipster
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import ch.ethz.dal.tinyir.lectures.PrecisionRecall

object System {
  def main(args: Array[String]) {
    val queriesPath = "src/main/resources/queries"
    val trainingQueriesPath = "src/main/resources/IR2015/tipster/qrels"
    val trainingTopicsPath = "src/main/resources/topics"
    val tipsterPath = "src/main/resources/IR2015/tipster/zips"

    var queries = loadTrainingQueries(queriesPath)
    println(queries)
    var groundTruth = loadGroundTruth(trainingQueriesPath)
    
    /*val d1 = new StringDocument(1,"mr sherlock holmes who was usually very late")
    val d0 = new StringDocument(0,"i can tell a moriaty when i see one said holmes")  
    val stream : Stream[StringDocument] = List(d1,d0).toStream
    val idx = new FreqIndex(stream)    
    idx.index.foreach{ case (d,lst) => println(d + ": " + lst.mkString(" "))}     
    val q = List("a","i")
    println(q.mkString(" ") + " = " + idx.results(q).mkString(" "))*/
    
    val topics = new TipsterTopicParser(trainingTopicsPath)
    topics.parse()
    
    val num = 100
    var alerts = Map[Int, AlertsTipster]()
    val tipster = new TipsterCorpusIterator(tipsterPath)
    val sw = new StopWatch; sw.start
    var iter = 0
    for (doc <- tipster) {
      for (q <- queries) {
        if (iter == 0) {
          println("Init topic " + q._1)
          alerts(q._1) = new AlertsTipster(q._2.origQuery,num)
        }
        alerts(q._1).process(doc.name, doc.tokens)
        if (iter % 20000 ==0) {
          println("Iteration = " + iter + " and topic " + q._1)
          alerts(q._1).results.foreach(println)    
        }
      }
      iter += 1
    }
    sw.stop
    println("Stopped time = " + sw.stopped)
    alerts(51).results.take(5).foreach(println)  
    
    val rel = groundTruth.judgements.get("51").get.toSet
    val ret = alerts(51).results.map(r => r.title)
    val pr = new PrecisionRecall(ret,rel)
    println(pr.relevIdx.mkString(" "))
    println(pr.precs.mkString(" "))
    println(pr.aps)
    println(pr.iprecs.mkString(" "))
    println(pr.iaps)
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
  
  def loadGroundTruth(groundTruthPath: String): TipsterGroundTruth = {
    val t = new TipsterGroundTruth(groundTruthPath)
    t.judgements.foreach(j => println("Topic "+j._1 +": "+j._2.size+" judgements found."))
    return t
  }
}