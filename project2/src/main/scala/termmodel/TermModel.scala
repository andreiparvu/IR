package termmodel

import ch.ethz.dal.tinyir.io.TipsterStream
import scala.collection.mutable.ListBuffer

object TermModel {

  def main(args: Array[String]) {
    val docs = new TipsterStream("/IR2015/zips")
    val queryDoc = scala.io.Source.fromFile("/IR2015/queries")
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

    val alerts = new AlertsTfIdf(queries.toList, 10)

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