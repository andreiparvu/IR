import scala.collection.mutable.HashMap

import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.processing.Tokenizer

class AlertsCosine(queries: Map[Int, Query], n: Int) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val df = new HashMap[String, Int]().withDefaultValue(0)

  var tfSum = 0
  var nrDocs = 0

  override def processDocument(doc: String) {
    nrDocs += 1
    tf.clear()
    for (w <- Tokenizer.tokenize(doc).map(_.toLowerCase)) {
      tf.update(w, tf(w) + 1)
      tfSum += 1
    }

    for (w <- tf.keys) {
      df.update(w, df(w) + 1)
    }
  }

  def tf_idf(word: String, tfs: Map[String, Int]): Double = {
    tfs(word) * Math.log((nrDocs.toDouble + 1) / (df(word).toDouble + 1))
  }

  override def computeScore(query: String): Double = {
    val qtf = Tokenizer.tokenize(query).groupBy(identity).mapValues(l => l.length)
    val qtf_idf = qtf.map{ case(w, v)  => tf_idf(w, qtf.toMap)}
    val doctf_idf = qtf.map{ case(w, v)  => tf_idf(w, tf.toMap)}
    val qLen = qtf_idf.map(x => x*x).sum.toDouble  // Euclidian norm
    val docLen = doctf_idf.map(x => x*x).sum.toDouble  // Euclidian norm 
    //Cosine between -1 and 1 (add one to have only positive values)
    1 + qtf.map{ case(w, v)  => tf_idf(w, tf.toMap) * tf_idf(w, qtf)}.sum / (docLen * qLen)
  }
}