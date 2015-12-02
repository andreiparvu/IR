import ch.ethz.dal.tinyir.alerts.ScoredResult
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.TipsterParse
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import ch.ethz.dal.tinyir.alerts.Query
import scala.collection.mutable.HashSet

class AlertsIDF(queries: Map[Int, Query], n: Int, tokenizer: Tokenizer) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val df = new HashMap[String, Int]().withDefaultValue(1)

  var nrDocs = 0

  override def preProcess(doc: String) {
    val terms = new HashSet[String]();

    for (w <- tokenizer.tokenize(doc).map(_.toLowerCase)) {
      terms += w
    }

    for (w <- terms) {
      df.update(w, df(w) + 1)
    }
    nrDocs += 1
  }

  override def processDocument(doc: String) {}

  def tf_idf(word: String): Double = {
    Math.log((nrDocs.toDouble + 1) / (df(word).toDouble + 1))
  }

  override def computeScore(query: String): Double = {
    tokenizer.tokenize(query).map(q => tf_idf(q.toLowerCase)).sum
  }
}