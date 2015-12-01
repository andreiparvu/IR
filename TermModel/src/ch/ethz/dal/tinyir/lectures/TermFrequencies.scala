package ch.ethz.dal.tinyir.lectures

import Math.log10
object TermFrequencies {

  def tf(doc: List[String]): Map[String, Int] =
    doc.groupBy(identity).mapValues(l => l.length)

  def logtf(doc: List[String]): Map[String, Double] =
    logtf(tf(doc))

  def atf(doc: List[String]): Map[String, Double] = {
    val atf = tf(doc)
    atf.mapValues(f => 0.5 * f / atf.values.max.toDouble + 0.5)
  }

  def logtf(tf: Map[String, Int]): Map[String, Double] =
    tf.mapValues(v => log2((v.toDouble + 1) / (tf.values.sum + 1)) + 1.0)

  def idf(df: Map[String, Int], n: Int): Map[String, Double] =
    //Smoothing... in case term not in corpus
    df.mapValues(v => log10(n) - log10(v + 1))

  def log2(x: Double) = log10(x) / log10(2.0)
  
  def languageModelScore(tf: Map[String, Int], df: Map[String, Int], n: Int) = {
     -logtf(tf).map{case(t, f) => f * idf(df, n)(t)}.sum
  }
  
  def termModelScore(qtf: Map[String, Int], dtf: Map[String, Int], df: Map[String, Int], n: Int): Double = {
    /*tfs = doc.groupBy(identity).mapValues(l => l.length)
    qtfs = qterms.map(q => (q, tfs.getOrElse(q, 0))).toMap 
    val numTermsInCommon = qtfs.filter{case(x, y) => y > 0}.size
    val docLen = tfs.values.map(x => x*x).sum.toDouble  // Euclidian norm
    val queryLen = qterms.length.toDouble  
    val termOverlap = qtfs.filter{case(x, y) => y > 0}.values.sum.toDouble / (docLen * queryLen)
    
    // top ordering is by terms in common (and semantics)
    // integer range from 0...qterms.length
    // on top of this a tf-based overlap score in range [0;1[ is added
    numTermsInCommon + termOverlap*/
    
    
    //Cosine distance between document d and query
    val q = logtf(qtf).map{case(t, f) => (t, f * idf(df, n)(t))}
    val d = logtf(dtf).map{case(t, f) => (t, f * idf(df, n)(t))}
    val dLen = d.values.map(x => x*x).sum.toDouble  // Euclidian norm
    val qLen = q.values.size.toDouble 
    if (dLen == 0) {
      return -1
    }
    else {
      return q.map{case(x,y) => d.getOrElse(x, 0.0) * y}.sum / (dLen * qLen)
    }
  }

  def main(args: Array[String]) = {
    val query: Set[String] = Set("green", "blue", "powder")
    val doc: List[String] = List("green", "blue", "red", "green")

    val score = query.flatMap(logtf(doc) get).sum
    println(score)
  }
}