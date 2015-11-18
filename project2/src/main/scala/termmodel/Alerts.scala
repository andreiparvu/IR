package termmodel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import ch.ethz.dal.tinyir.alerts.ScoredResult

abstract class Alerts(queries: List[String], n: Int) {
  protected val heaps = ArrayBuffer[PriorityQueue[ScoredResult]]()

  for (_ <- 0 to queries.size) {
    heaps += new PriorityQueue[ScoredResult]()(Ordering.by(score))
  }

  def processDocument(doc: String)
  def computeScore(q: String): Double

  // score a document and try to add to results
  def process(title: String, doc: String) {
    processDocument(doc)
    queries.zipWithIndex foreach {
      case (q, i) => {
        add(heaps(i), ScoredResult(title, computeScore(q)))
      }
    }
  }

  // get top n results (or m<n, if not enough docs processed)
  def results = heaps.map(_.toList.sortBy(res => -res.score))

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