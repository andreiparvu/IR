import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing.Tokenizer
import com.github.aztek.porterstemmer.PorterStemmer
import ch.ethz.dal.tinyir.alerts.Query

class AlertsMLEStem(queries: Map[Int, Query], n: Int) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val cf = new HashMap[String, Int]().withDefaultValue(0)

  var tfSum = 0
  var cfSum = 0

  override def preProcess(doc: String) {
    for (w <- Tokenizer.tokenize(doc).map(PorterStemmer.stem(_))) {
      cf.update(w, cf(w) + 1)
      cfSum += 1
    }

  }

  override def processDocument(doc: String) {
    tf.clear()
    tfSum = 0
    for (w <- Tokenizer.tokenize(doc).map(PorterStemmer.stem(_))) {
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
    Tokenizer.tokenize(query).map(q => mle(PorterStemmer.stem(q))).sum
  }
}