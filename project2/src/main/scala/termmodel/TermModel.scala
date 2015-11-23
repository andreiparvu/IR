package termmodel

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

object TermModel {

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

    for (q <- queryDoc.getLines()) {
      try {
        val id = q.toInt
        queryIds += id
      } catch {
        case e: Exception => queries += q
      }
    }

    val alerts = new AlertsSynonyms(queries.toList, 10, synonyms, synonymGroup)

    val iter = new TipsterCorpusIterator("/home/andrei/Documents/IR/project2/zips")

    var i = 1
    while(iter.hasNext) {
    	val doc = iter.next
    			alerts.process(doc.name, doc.body)

    			i += 1
    			if (i % 1000 == 0) {
    				println(i)
    			}
    }

    alerts.results.zip(queryIds).foreach {
      case (results, id) => {
        results.foreach {
          x => println(id + " " + x.title + " " + x.score)
        }
      }
    }
  }
}