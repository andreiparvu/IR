import scala.collection.mutable.HashMap

import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.processing.Tokenizer

class AlertsSynonyms(queries: Map[Int, Query], n: Int, synonyms: HashMap[Int, List[String]],
    synonymGroup: HashMap[String, Int]) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val cf = new HashMap[String, Int]().withDefaultValue(0)

  var tfSum = 0
  var cfSum = 0

  override def preProcess(doc: String) {}
  override def processDocument(doc: String) {
    tf.clear()
    tfSum = 0
    for (w <- Tokenizer.tokenize(doc).map(_.toLowerCase)) {
      tf.update(w, tf(w) + 1)
      tfSum += 1
    }

    for ((w, freq) <- tf) {
      cf.update(w, cf(w) + freq)
      cfSum += freq
    }
  }

  private val lambda = 0.5

  def mle(query: String): Double = {
    if (synonymGroup.contains(query)) {
      synonyms(synonymGroup(query)).map(
          s => Math.log((1.0 - lambda) * tf(query).toDouble / tfSum +
        lambda * cf(query).toDouble / cfSum)).max
    } else {
      Math.log((1.0 - lambda) * tf(query).toDouble / tfSum +
          lambda * cf(query).toDouble / cfSum)
    }
  }

  override def computeScore(query: String): Double = {
    Tokenizer.tokenize(query).map(q => mle(q.toLowerCase)).sum
  }
}