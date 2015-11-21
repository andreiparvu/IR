package main.scala.termmodel

import scala.collection.mutable.MutableList
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.processing._
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import ch.ethz.dal.tinyir.indexing.FreqIndex

object System {
  def main(args: Array[String]) {
    val queriesPath = "src/main/resources/queries"
    val trainingQueriesPath = "src/main/resources/IR2015/tipster/qrels"
    val trainingTopicsPath = "src/main/resources/topics"

    var queries = loadTrainingQueries(queriesPath)
    println(queries)
    var groundTruth = loadGroundTruth(trainingQueriesPath)
    
    val d1 = new StringDocument(1,"mr sherlock holmes who was usually very late")
    val d0 = new StringDocument(0,"i can tell a moriaty when i see one said holmes")  
    val stream : Stream[StringDocument] = List(d1,d0).toStream
    val idx = new FreqIndex(stream)    
    idx.index.foreach{ case (d,lst) => println(d + ": " + lst.mkString(" "))}     
    val q = List("a","i")
    println(q.mkString(" ") + " = " + idx.results(q).mkString(" "))
    
    val topics = new TipsterTopicParser(trainingTopicsPath)
    topics.parse()
  }

  def loadTrainingQueries(queriesPath: String): Map[Int, Query] = {
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