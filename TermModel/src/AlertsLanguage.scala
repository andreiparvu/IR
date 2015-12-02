import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.alerts.Query
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable.HashSet


class AlertsLanguage(queries: Map[Int, Query], n: Int, tokenizer: Tokenizer) extends Alerts(queries, n) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  val cf = new HashMap[String, Int]().withDefaultValue(0)
  val df = new HashMap[String, Int]().withDefaultValue(0)

  var tfSum = 0
  var cfSum = 0
  var dfSum = 0
  var nrDocs = 0
  
  override def preProcess(doc: String) {
    nrDocs += 1
    val terms = new HashSet[String]();

    for (w <- tokenizer.tokenize(doc).map(_.toLowerCase)) {
      terms += w
    }

    for (w <- terms) {
      df.update(w, df(w) + 1)
    }
  }

  override def processDocument(doc: String) {
    tf.clear()
    tfSum = 0
    
    nrDocs += 1
    
    //println(doc)
    for (w <- tokenizer.tokenize(doc)) {
      //println(w)
      tf.update(w, tf(w) + 1)
      tfSum += 1
    }

    for ((w, freq) <- tf) {
      cf.update(w, cf(w) + freq)
      if (freq > 0) {
        df.update(w, df(w) + 1)
        dfSum += 0
      }
      cfSum += freq
    }
  }
  
  
  

  private val lambda = 0.5

  def mle(query: String): Double = {
    Math.log((1.0 - lambda) * tf(query).toDouble / tfSum +
        lambda * cf(query).toDouble / cfSum)
  }
  
  def language(word: String): Double = {
    tf(word).toDouble * Math.log((nrDocs + 1) / (df(word) + 1)) 
  }
  
  override def computeScore(query: String): Double = {
    tokenizer.getTokens(query).map(q => language(q)).sum
  }
}