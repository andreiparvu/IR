import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.alerts.Query

class AlertsMLE(queries: Map[Int, Query], n: Int, tokenizer: Tokenizer) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val cf = new HashMap[String, Int]().withDefaultValue(0)

  var tfSum = 0
  var cfSum = 0

  override def preProcess(doc: String) {
    for (w <- tokenizer.tokenize(doc).map(_.toLowerCase)) {
      cf.update(w, cf(w) + 1)
      cfSum += 1
    }

  }
  override def processDocument(doc: String) {
    tf.clear()
    tfSum = 0
    for (w <- tokenizer.tokenize(doc).map(_.toLowerCase)) {
      tf.update(w, tf(w) + 1)
      tfSum += 1
    }
  }

  private val lambda = 0.5

  def mle(query: String): Double = {
    Math.log((1.0 - lambda) * tf(query).toDouble / tfSum +
        lambda * cf(query).toDouble / cfSum)
  }
  override def computeScore(query: String): Double = {
    tokenizer.tokenize(query).map(q => mle(q.toLowerCase)).sum
  }
}