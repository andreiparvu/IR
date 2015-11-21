
package termmodel

import ch.ethz.dal.tinyir.alerts.ScoredResult
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.processing._
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer

class AlertsTfIdf(queries: List[String], n: Int) {
  val tf = new HashMap[String, Int]().withDefaultValue(0)
  //Why default 1?
  val df = new HashMap[String, Int]().withDefaultValue(1)

  //Why set to 1?
  var nrDocs = 1

  //So heaps is an array of queues containing scoredresult
  private val heaps = ArrayBuffer[PriorityQueue[ScoredResult]]()

  //Initializing the array-> for each query
  for (_ <- 0 to queries.size) {
    heaps += new PriorityQueue[ScoredResult]()(Ordering.by(score))
  }

  def processDocument(doc: String) {
    nrDocs += 1
    tf.clear()
    for (w <- Tokenizer.tokenize(doc).map(_.toLowerCase)) {
      tf.update(w, tf(w) + 1)
    }

    for (w <- tf.keys) {
      df.update(w, df(w) + 1)
    }
  }

  def tf_idf(word: String): Double = {
    Math.log(1 + tf(word).toDouble / tf.values.sum.toDouble) * Math.log(nrDocs.toDouble / df(word).toDouble)
  }

  def score(query: String): Double = {
    Tokenizer.tokenize(query).map(q => tf_idf(q.toLowerCase)).sum
  }

  // score a document and try to add to results
  def process(title: String, doc: String) {
    processDocument(doc)
    queries.zipWithIndex foreach {
      case (q, i) => {
        add(heaps(i), ScoredResult(title, score(q)))
      }
    }
  }

  // get top n results (or m<n, if not enough docs processed)
  def results = heaps.map(_.toList.sortBy(res => -res.score))

  // heap and operations on heap

  private def score(res: ScoredResult) = -res.score

  private def add(heap: PriorityQueue[ScoredResult], res: ScoredResult) {
    if (heap.size < n) { // heap not full
      heap += res
    } else if (heap.head.score < res.score) {
      heap.dequeue
      heap += res
    }
  }
}