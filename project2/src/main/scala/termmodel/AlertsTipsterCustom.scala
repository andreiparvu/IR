package main.scala.termmodel

import ch.ethz.dal.tinyir.alerts._
import ch.ethz.dal.tinyir.lectures.TermFrequencies
import scala.collection.mutable.Map

class AlertsTipsterCustom(q: String, n: Int, scoreType: String) extends AlertsTipster(q, n){
  var tfs = collection.immutable.Map[String, Int]().withDefaultValue(0)
  //The collection frequency of t is the number of 
  //occurrences of t in the collection, counting 
  //multiple occurrences
  var cfs = Map[String, Int]().withDefaultValue(0)
  var dfs = Map[String,Int]().withDefaultValue(0)
  var qcfs = Map[String, Int]().withDefaultValue(0)
  var qdfs = Map[String,Int]().withDefaultValue(0)
  var qtfs = collection.immutable.Map[String,Int]().withDefaultValue(0)
  val chosenScore = scoreType
  var nDoc: Int = 0
  var termScore: Double = 0
  
  // score a document and try to add to results
  override def process(title: String, doc: List[String]) : Boolean = {
    termScore = query.score(doc)
    nDoc += 1
    tfs = query.tfs
    qtfs = query.qtfs
    dfs ++= doc.distinct.map(t => t-> (1+dfs.getOrElse(t,0)))
    cfs ++= doc.map(t => t-> (1+cfs.getOrElse(t,0)))
    qdfs ++= qtfs.map{case(t, v) => t-> (1+dfs.getOrElse(t,0))}
    qcfs ++= qtfs.toMap.map{case(t, v) => t-> (v+qcfs.getOrElse(t,0))}
    println(qtfs)
    super.add(ScoredResult(title,processScore()))
  }
  
  def processScore(): Double = {
    if (chosenScore == "language") {
      return TermFrequencies.languageModelScore(qtfs, qdfs.toMap, nDoc)
    }
    else{
      //termScore
      
      //Using cosine distance
      return TermFrequencies.termModelScore(query.qterms.map(x => (x,1)).toMap, qtfs, qdfs.toMap, nDoc)
    }
  }
}