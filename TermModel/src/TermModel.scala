import ch.ethz.dal.tinyir.processing.ReutersRCVParse
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.io.ParsedXMLStream
import ch.ethz.dal.tinyir.io.ZipDirStream
import java.io.InputStream
import ch.ethz.dal.tinyir.io.DocStream
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Document => XMLDoc}
import ch.ethz.dal.tinyir.processing.TipsterParse
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.io.TipsterStream
import scala.collection.mutable.ListBuffer
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator
import java.nio.file.Paths
import java.nio.file.Files
import scala.collection.mutable.MutableList
import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.lectures._
import java.io.PrintWriter

object TermModel {

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
    t.judgements.foreach(j => println("Topic " + j._1 + ": " + j._2.size + " judgements found."))
    return t
  }

  def main(args: Array[String]) {
    val queryDoc = scala.io.Source.fromFile("/home/andrei/Documents/IR/project2/queries")
    val queryIds = new ListBuffer[Int]()
    val queries = new ListBuffer[String]()
    val synonymDoc = scala.io.Source.fromFile("/home/andrei/Documents/IR/project2/synonyms")

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

    val queriesPath = "/home/andrei/Documents/IR/project2/queries"
    val trainingQueriesPath = "/home/andrei/Documents/IR/project2/qrels"
    var queries2 = loadTrainingQueries(queriesPath)
    println(queries)
    var groundTruth = loadGroundTruth(trainingQueriesPath)

    for (q <- queryDoc.getLines()) {
      try {
        val id = q.toInt
        queryIds += id
      } catch {
        case e: Exception => queries += q
      }
    }

    val alerts = new AlertsMLEStem(queries2, 100)

    var iter = new TipsterCorpusIterator("/home/andrei/Documents/IR/project2/zips")

    var i = 1
    while(iter.hasNext) {
      val doc = iter.next
      alerts.preProcess(doc.body)

      i += 1
        if (i % 10000 == 0) {
          println(i)
        }
    }

    println("Preprocessing done")

    iter = new TipsterCorpusIterator("/home/andrei/Documents/IR/project2/zips")
    i = 1
    try {
      while(iter.hasNext) {
      	val doc = iter.next
  			alerts.process(doc.name, doc.body)

  			i += 1
  			if (i % 10000 == 0) {
  				println(i)
  			}
      }
    } catch {
      case (e: Exception) => {}
    }

    val pw = new PrintWriter("/home/andrei/results")
    var totalF1: Double = 0
    for (q <- queries2) {
      pw.write(q._1 + "\n")
      val rel = groundTruth.judgements.get(q._1 + "").get.toSet
      pw.write(rel.toString() + "\n")
      val ret_term = alerts.results(q._1)
      i += 1
      val t = ret_term.map(_.title).toSet
      pw.write(t.toString() + "\n")
      pw.write(ret_term + "\n")

      val truePos = (t & rel).size
      val precision = truePos.toDouble / t.size.toDouble
      val recall    = truePos.toDouble / Math.min(rel.size.toDouble, 100)

      pw.write(precision + "\n")
      pw.write(recall + "\n")
      totalF1 += precision * recall * 2 / (precision + recall)
    }

    pw.write(totalF1 / queries2.size + "")
    pw.flush()
    pw.close()
  }
}