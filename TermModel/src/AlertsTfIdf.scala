
import ch.ethz.dal.tinyir.alerts.ScoredResult
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.TipsterParse
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import ch.ethz.dal.tinyir.alerts.Query

class AlertsTfIdf(queries: Map[Int, Query], n: Int) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val df = new HashMap[String, Int]().withDefaultValue(1)

  var tfSum = 0
  var nrDocs = 1

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

  def tf_idf(word: String): Double = {
    Math.log(1 + tf(word).toDouble / tfSum) * Math.log(nrDocs.toDouble / df(word).toDouble)
  }

  override def computeScore(query: String): Double = {
    Tokenizer.tokenize(query).map(q => tf_idf(q.toLowerCase)).sum
  }
}