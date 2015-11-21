package termmodel

import ch.ethz.dal.tinyir.io.TipsterStream
import scala.collection.mutable.ListBuffer
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

object Main {

  def main(args: Array[String]) {
    val zipPath = "src/main/resources/IR2015/tipster/zips/"
    val queriesPath = "src/main/resources/queries"
    val trainingQueriesPath = "src/main/resources/IR2015/tipster/qrels"
    val trainingTopicsPath = "src/main/resources/IR2015/tipster/topics"
    
    if (Files.exists(Paths.get(zipPath))) {
      print("Found zips at " + zipPath + "\n")
    }
    
    
    val docs = new TipsterStream(zipPath)
    println("Number of zips " + docs.length)
    val queryDoc = scala.io.Source.fromFile(queriesPath)
    val queryIds = new ListBuffer[Int]()
    val queries = new ListBuffer[String]()

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