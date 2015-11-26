package ch.ethz.dal.tinyir.alerts

import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords

class Query (query: String) {  
  val origQuery = query
  //Removing stop words as well
  val qterms = StopWords.filter(Tokenizer.splitWords(query).distinct)
  val length = qterms.length
  var tfs = Map[String,Int]()
  var qtfs = Map[String,Int]()

  def score (doc: List[String]) : Double = {
    tfs = doc.groupBy(identity).mapValues(l => l.length)
    qtfs = qterms.map(q => (q, tfs.getOrElse(q, 0))).toMap 
    val numTermsInCommon = qtfs.filter{case(x, y) => y > 0}.size
    val docLen = tfs.values.map(x => x*x).sum.toDouble  // Euclidian norm
    val queryLen = qterms.length.toDouble  
    val termOverlap = qtfs.filter{case(x, y) => y > 0}.values.sum.toDouble / (docLen * queryLen)
    
    // top ordering is by terms in common (and semantics)
    // integer range from 0...qterms.length
    // on top of this a tf-based overlap score in range [0;1[ is added
    numTermsInCommon + termOverlap
  }
}