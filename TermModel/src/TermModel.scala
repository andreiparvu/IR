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

object TermModel {

  def main(args: Array[String]) {
    val docs = new TipsterStream("/home/andrei/Documents/IR/project2/zips")
    val queryDoc = scala.io.Source.fromFile("/home/andrei/Documents/IR/project2/queries")
    val queryIds = new ListBuffer[Int]()
    val queries = new ListBuffer[String]()

    for (q <- queryDoc.getLines()) {
      try {
        val id = q.toInt
        queryIds += id
      } catch {
        case e: Exception => queries += q
      }
    }

    val alerts = new AlertsMLE(queries.toList, 10)

    var i = 1
    try {
      for (d <- docs.stream) {
        alerts.process(d.name, d.body)

        if (i % 100 == 0) {
          println("Processed " + i)
        }
        i += 1
        if (i == 1000) {
          throw new Exception()
        }
      }
    } catch {
      case e: Exception => {}
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