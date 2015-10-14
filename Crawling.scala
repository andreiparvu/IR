

/**
 * @author Matthias Fuhr, Andrei Parvu
 */

import scala.io.Source
import java.net.URL
import java.io._
import scala.collection.JavaConversions._
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.HashSet

case class langModel(name : String, tokenDistribution : Map[String, Double])

object Crawling {
  type MutList[T] = scala.collection.mutable.ListBuffer[T]
  type MutMap[X, Y] = scala.collection.mutable.HashMap[X, Y]

  var urls = new HashSet[String]
  var content = new HashSet[Int]

  var studentOccurence = 0
  var nearDuplicates = 0
  var exactDuplicates = 0
  val validPages = new AtomicInteger
  var englishPagesOccurence = 0

  val fileLangDE = "languageModel_DE.txt"
  val fileLangEN = "languageModel_EN.txt"
  val langModel_DE = new langModel(name = "german" , tokenDistribution = Source.fromFile(fileLangDE).getLines().toList.map(_.split("\t")).map{ case Array(k:String, v:String) => (k.toString, v.toDouble) }.toMap )
  val langModel_EN = new langModel(name = "english" , tokenDistribution = Source.fromFile(fileLangEN).getLines().toList.map(_.split("\t")).map{ case Array(k:String, v:String) => (k.toString, v.toDouble) }.toMap )
  val langModel = List(langModel_EN, langModel_DE)

  def defaultDouble = (s: String) => 0.toDouble

  def genPermutation(limit: Int): List[Int] = {
    // random permutation generator, used for hashing
    val r = new scala.util.Random()

    r.shuffle(0 to limit toList)
  }

  def applyPermutation(l: List[Int], perm: List[Int]): List[Int] = {
    for (x <- perm) yield l(x)
  }

  // we can handle having more permutations and considering only a part of the hash
  val HASH_PREFIX = 25
  val NUM_TABLES = 3 // number of permutations
  val HASH_BITS = 32

  val perms = new MutList[List[Int]]()
  val tables = new MutList[scala.collection.mutable.Set[String]]()

  val NUM_SHINGLES = 7

  var host = ""

  def main(args: Array[String]) {
    val url = new URL(args(0))
    host = url.getHost

    for (_ <- 1 to NUM_TABLES) {
      perms += genPermutation(HASH_BITS - 1)
      tables += new HashSet
    }

    crawlPage(url)

    println("Distinct URLs found: " + validPages.get)
    println("Exact duplicates found: " + exactDuplicates)
    println("Near duplicates found: " + nearDuplicates)
    println("Number of unique pages mostly in English: " + englishPagesOccurence)
    println("Term frequency of \"student\": " + studentOccurence)
  }

  var queue = new java.util.concurrent.LinkedBlockingQueue[URL]

  val NUM_THREADS = 1

  def addToTables(hash: List[Int]) {
    // go through each permutation, apply it and add the hash to the table
    for (i <- 0 to NUM_TABLES - 1) {
      val shortHash = applyPermutation(hash, perms(i)).take(HASH_PREFIX).mkString

      tables(i) += shortHash
    }
  }

  def findIfDuplicate(hash: List[Int]): Boolean = {
    // if we find the hash in at least one table, return true
    for (i <- 0 to NUM_TABLES - 1) {
      val shortHash = applyPermutation(hash, perms(i)).take(HASH_PREFIX).mkString

      if (tables(i).contains(shortHash)) {
        return true
      }
    }

    false
  }

  def crawlPage(url: URL) {
    queue.add(url)
    urls += url.getPath

    // we support multi-threading
    val threads = scala.collection.mutable.ArrayBuffer[Worker]()

    for (i <- 1 to NUM_THREADS) {
      threads += new Worker
      threads.last.start()
    }

    threads.foreach { _.join() }

    println("We have " + urls.size)
  }

  class Worker extends Thread {
    def genHash(l: List[String]): List[Int] = {
      // reduce a list of strings to a single hash
      val hashed = l.map(s => String.format("%32s", Integer.toBinaryString(s.hashCode())).replace(' ', '0').toList)

      var trans_hash = hashed.map(l =>
        l.map {
          case '0' => -1
          case '1' => 1})

      var sum = trans_hash.reduceLeft(
          (x, y) => (x, y).zipped.map(_ + _))

      sum.map(x => if (x >= 0) 1 else 0)
    }

    def createShingles(content: String): List[String] = {
      val tokens = content.split("[ .,;:?!\t\n\r\f<>]+").toList

      val shingles = tokens.sliding(NUM_SHINGLES)

      shingles.toSet.map((x: List[String]) => x.mkString(" ")).toList
    }

    def analyzeContent(pagecontent: String, url: String) {
      var exists = false
      content.synchronized {
        exists = content.contains(pagecontent.hashCode)
        if (exists) {
          exactDuplicates += 1
        }
        content += pagecontent.hashCode()
      }
      if (exists) {
        return
      }

      // keep only the main content
      var contentRegex = """(?s)<(section|div)[^>]*contentMain">.*</\1>""".r
      var mainContent = contentRegex.findFirstIn(pagecontent).getOrElse("");

      // eliminate tags
      var tags = "<[^>]*>".r

      mainContent = tags.replaceAllIn(mainContent, "")

      if (mainContent.size == 0) {
        return
      }

      val hash = genHash(createShingles(mainContent))

      tables.synchronized {
        if (findIfDuplicate(hash)) {
          nearDuplicates += 1
          return
        } else {
          val lang = if (mainContent.size == 0) "unknown" else classifyLanguage(mainContent, langModel)
          if (lang == "english") {
            englishPagesOccurence += 1
            var student = "(?i)student".r

            studentOccurence += student.findAllIn(mainContent).size
          }

          addToTables(hash)
        }
      }
    }

    def classifyLanguage(pagecontent: String, languageModels: List[langModel]): String = {
      val tokenL = aggregateTokensWord(pagecontent)
      var num: Double = 0
      var MaximumLikelihood: scala.collection.mutable.MutableList[Double] = scala.collection.mutable.MutableList()
      for (m <- languageModels) {
        var ml: Double = 0
        for (t <- tokenL) {
          num = m.tokenDistribution.applyOrElse(t._1, defaultDouble)
          //this seems to work better than using the log-likelihood
          if (num == 0) ml += 0 else ml += num * t._2
        }
        MaximumLikelihood += ml
      }
      val l = MaximumLikelihood.zipWithIndex.maxBy(_._1)._2
      languageModels(l).name
    }

    def aggregateTokensWord(text : String) : scala.collection.mutable.Map[String, Int] = {
      val textClean : String = text.replaceAll( "[.,??\"!-;\\t\\n\\r\\f_]" , " " ).toLowerCase
      val tokens = textClean.split("\\W+")
      val lCounts = tokens.groupBy(identity).mapValues(_.size)
      val lCountsMutable : scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()
      for(l <- lCounts){
        lCountsMutable.update(l._1, l._2)
      }
      lCountsMutable
    }

    def explorePageForURL(url: URL): List[URL] = {
      try {
        val pagecontent = Source.fromURL(url)("UTF-8").mkString

        validPages.incrementAndGet()

        analyzeContent(pagecontent, "http://" + url.getHost + url.getPath);

        val testPattern = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r

        val linksAsList = testPattern.findAllIn(pagecontent).toList.filter(s => s.endsWith(".html\""))
        val linksAsStr = linksAsList.map(s => s.replace("href=", "").replace("\"", "").replace("HREF=", ""))

        linksAsStr.map(s => new URL(url, s))
      } catch {
        case e: java.io.IOException => List()
        case e: java.nio.charset.MalformedInputException => List()
        case e: java.io.FileNotFoundException => List()
      }
    }

    override def run() {
      while (true) {
        val url = queue.poll(4, java.util.concurrent.TimeUnit.SECONDS)

        if (url == null) {
          // Thread exit
          return
        }

        for (new_url <- explorePageForURL(url)) {
          urls.synchronized {
            if (new_url.getHost == host && !urls.contains(new_url.getPath)) {
              urls += new_url.getPath
              queue.add(new_url)
            }
          }
        }
      }
    }
  }
}