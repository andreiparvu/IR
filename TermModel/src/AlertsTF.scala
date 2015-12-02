import ch.ethz.dal.tinyir.alerts.ScoredResult
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.TipsterParse
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import ch.ethz.dal.tinyir.alerts.Query
import scala.collection.mutable.HashSet

class AlertsTF(queries: Map[Int, Query], n: Int, tokenizer: Tokenizer) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val df = new HashMap[String, Int]().withDefaultValue(1)

  var tfSum = 0

  override def preProcess(doc: String) {}

  override def processDocument(doc: String) {
    tf.clear()
    for (w <- tokenizer.tokenize(doc).map(_.toLowerCase)) {
      tf.update(w, tf(w) + 1)
      tfSum += 1
    }
  }

  def tf_idf(word: String): Double = {
    Math.log(1 + tf(word).toDouble / tfSum) / Math.log(2)
  }

  override def computeScore(query: String): Double = {
    tokenizer.tokenize(query).map(q => tf_idf(q.toLowerCase)).sum
  }
}