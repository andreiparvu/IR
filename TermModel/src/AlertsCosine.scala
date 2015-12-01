import scala.collection.mutable.HashMap

import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable.HashSet

class AlertsCosine(queries: Map[Int, Query], n: Int) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val df = new HashMap[String, Int]().withDefaultValue(0)

  var tfSum = 0
  var nrDocs = 0

  override def processDocument(doc: String) {
    tf.clear()
    for (w <- Tokenizer.tokenize(doc.toLowerCase)) {
      tf.update(w, tf(w) + 1)
      tfSum += 1
    }
  }

  def tf_idf(word: String, tf_val: Int): Double = {
    tf_val * Math.log((nrDocs.toDouble + 1) / (df.getOrElse(word, 0).toDouble + 1))
  }
  
  override def preProcess(doc: String) {
    val terms = new HashSet[String]();

    for (w <- Tokenizer.tokenize(doc.toLowerCase)) {
      terms += w
    }

    for (w <- terms) {
      df.update(w, df(w) + 1)
    }
    nrDocs += 1
  }

  override def computeScore(query: String): Double = {
    val qtf = Tokenizer.tokenize(query.toLowerCase).groupBy(identity).mapValues(l => l.length)
    val qtf_idf = qtf.map{ case(w, v)  => tf_idf(w, v)}
    val doctf_idf = qtf.map{ case(w, v)  => tf_idf(w, tf.getOrElse(w, 0))}
    val qLen = Math.sqrt(qtf_idf.map(x => x*x).sum.toDouble)  // Euclidian norm
    val docLen = Math.sqrt(doctf_idf.map(x => x*x).sum.toDouble)  // Euclidian norm 
    //Cosine between -1 and 1 (add one to have only positive values)
    
    if (docLen == 0){
      return 0
    }
    else {
      1 + (for ( (q, d) <- (qtf_idf zip doctf_idf)) yield q * d).sum / (docLen * qLen)
    }
  }
}