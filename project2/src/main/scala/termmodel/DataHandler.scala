package main.scala.termmodel

import scala.collection.mutable.ListBuffer
import ch.ethz.dal.tinyir.alerts.Query

class DataHandler {
  var queryIds = new ListBuffer[Int]()
  var queries = new ListBuffer[Query]()
  def queriesPath = "src/main/resources/queries"
  def trainingQueriesPath = "src/main/resources/IR2015/tipster/qrels"
  def trainingTopicsPath = "src/main/resources/IR2015/tipster/topics"
  
  def loadQueries() {
    val queryDoc = scala.io.Source.fromFile(queriesPath)
    for (q <- queryDoc.getLines()) {
      print("Query: " + q + "\n")
      val numeric_regex = "\\d+".r
      q match {
        case numeric_regex(group) => {
          queryIds += group.toInt
        }
        case _ => {
          queries += new Query(q)
        }
      }
    }
  }
}