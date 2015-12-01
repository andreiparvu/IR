import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import ch.ethz.dal.tinyir.alerts.ScoredResult
import scala.collection.mutable.HashMap
import ch.ethz.dal.tinyir.alerts.Query

abstract class Alerts(queries: Map[Int, Query], n: Int) {
  protected val heaps = HashMap[Int, PriorityQueue[ScoredResult]]()

  for ((k, v) <- queries) {
    heaps += k -> new PriorityQueue[ScoredResult]()(Ordering.by(score))
  }

  def processDocument(doc: String)
  def computeScore(q: String): Double

  def preProcess(doc: String);

  // score a document and try to add to results
  def process(title: String, doc: String) {
    processDocument(doc)
    for ((k, v) <- queries) {
      add(heaps(k), ScoredResult(title, computeScore(v.origQuery)))
    }
  }

  // get top n results (or m<n, if not enough docs processed)
  def results = heaps.mapValues(_.toList.sortBy(res => -res.score))

  // heap and operations on heap

  private def score(res: ScoredResult) = -res.score

  private def add(heap: PriorityQueue[ScoredResult], res: ScoredResult) {
    if (heap.size < n)  { // heap not full
      heap += res
    } else if (heap.head.score < res.score) {
      heap.dequeue
      heap += res
    }
  }
}