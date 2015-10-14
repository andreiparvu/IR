import scala.io.Source
import java.net.URL
import java.io._
import scala.collection.JavaConversions._
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

case class langModel(name : String, tokenDistribution : Map[String, Double])

object Crawling {
  type MutList[T] = scala.collection.mutable.ListBuffer[T]
  type MutMap[X, Y] = scala.collection.mutable.HashMap[X, Y]

  var urls = new ConcurrentHashMap[String, String]
  var eliminated = new ConcurrentHashMap[Int, Int]().withDefaultValue(0)
  var content = new ConcurrentHashMap[Int, Int]().withDefaultValue(0)

  val studentOccurence = new AtomicInteger
  val englishPagesOccurence = new AtomicInteger

  val hashes = new MutList[List[Int]]()

  val fileLangDE = "C:\\Users\\mafu\\Dropbox\\CAS ETH Software Engineering\\HS2015_InformationRetrieval\\Exercises\\Ex02\\languageModel_DE.txt"
  val fileLangEN = "C:\\Users\\mafu\\Dropbox\\CAS ETH Software Engineering\\HS2015_InformationRetrieval\\Exercises\\Ex02\\languageModel_EN.txt"
  val langModel_DE = new langModel( name = "german" , tokenDistribution = Source.fromFile(fileLangDE).getLines().toList.map(_.split("\t")).map{ case Array(k:String, v:String) => (k.toString, v.toDouble) }.toMap )
  val langModel_EN = new langModel( name = "english" , tokenDistribution = Source.fromFile(fileLangEN).getLines().toList.map(_.split("\t")).map{ case Array(k:String, v:String) => (k.toString, v.toDouble) }.toMap )
  val langModel = List(langModel_EN, langModel_DE)

  def defaultDouble = (s: String) => 0.toDouble

  def genPermutation(limit: Int): List[Int] = {
    val r = new scala.util.Random()

    r.shuffle(0 to limit toList)
  }

  def applyPermutation(l: List[Int], perm: List[Int]): List[Int] = {
    for (x <- perm) yield l(x)
  }

  val HASH_PREFIX = 32
  val NUM_TABLES = 1 // number of permutations
  val HASH_BITS = 32

  val perms = scala.collection.mutable.ListBuffer[List[Int]]()
  val tables = new MutList[scala.collection.mutable.Map[String, MutList[Int]]]()

  val NUM_SHINGLES = 7

  var host = ""

  def main(args: Array[String]) {
    //val url = new URL(args(0))
    val surl : String = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
    val url = new URL(surl)
    host = url.getHost

    crawlPage(url)

    println("Distinct URLs found: " + urls.size)
    val exactDuplicates = content.foldLeft(0){(sum: Int, el: (Int, Int)) => sum + el._2 - 1}
    println("Exact duplicates found: " + exactDuplicates)
    constructTables(hashes.toList)

    var exploredIndexes = new scala.collection.mutable.HashSet[Int]
    var nearDuplicates = eliminated.foldLeft(0){(sum: Int, el: (Int, Int)) => sum + el._2 - 1}

    hashes.zipWithIndex foreach {
      case (hash, i) =>
        if (!exploredIndexes.contains(i)) {
          findNearDuplicates(hash) foreach {
            index =>
              nearDuplicates += 1
              exploredIndexes += index
          }
        }
    }

    println("Near duplicates found: " + (nearDuplicates - exactDuplicates))
    println("Term frequency of \"student\": " + studentOccurence.get)


    println("Number of unique pages mostly in English: " + englishPagesOccurence.get)
  }

  var queue = new java.util.concurrent.LinkedBlockingQueue[URL]

  var lock = new Object
  val NUM_THREADS = 10

  def constructTables(hashes: List[List[Int]]) {
    for (_ <- 1 to NUM_TABLES) {
      perms += genPermutation(HASH_BITS - 1)
      val curMap = new MutMap[String, MutList[Int]]()

      hashes.zipWithIndex.foreach {
        case (hash, index) =>
          val temp = applyPermutation(hash, perms.last).take(HASH_PREFIX).mkString
          if (!curMap.contains(temp))
            curMap += temp -> new MutList[Int]()
          curMap(temp) += index
      }

      tables.append(curMap)
    }
  }

  def findNearDuplicates(hash: List[Int]): Set[Int] = {
    val results = scala.collection.mutable.Set[Int]()

    for (i <- 0 to NUM_TABLES - 1) {
      val shortHash = applyPermutation(hash, perms(i)).take(HASH_PREFIX).mkString

      for (x <- tables(i)(shortHash)) {
        results += x
      }
    }

    results.toSet
  }

  def crawlPage(url: URL) {
    queue.add(url)
    urls.put(url.getPath, url.getPath)
    val threads = scala.collection.mutable.ArrayBuffer[Worker]()

    for (i <- 1 to NUM_THREADS) {
      threads += new Worker()
      threads.last.start()
    }

    threads.foreach { _.join() }

    println("We have " + urls.size)
  }

  class Worker extends Thread {
    def genHash(l: List[String]): List[Int] = {
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

    def analyzeContent(pagecontent: String) {
      content.update(pagecontent.hashCode(), 1 + content(pagecontent.hashCode()))

      var student = "(?i)student".r

      studentOccurence.addAndGet(student.findAllIn(pagecontent).size)

      var paragraphs = "<p[^>]*>.*</p>".r

      var page_content = new MutList[String]()

      for (t <- paragraphs.findAllIn(pagecontent)) {
        var tags = "<[^>]*>".r

        page_content += tags.replaceAllIn(t, "")
      }

      val shingles = createShingles(page_content.mkString("\n"))

      if (page_content.size != 0 && shingles.size > 20) {
        hashes.synchronized {
          hashes += genHash(shingles)
        }
      } else {
        eliminated.update(pagecontent.hashCode(), 1 + eliminated(pagecontent.hashCode()))
      }

      val lang = if (page_content.size == 0) "unknown" else classifyLanguage(page_content.toString, langModel)
      if(lang == "english") englishPagesOccurence.addAndGet(1)
    }

    def classifyLanguage(pagecontent : String, languageModels : List[langModel]): String = {
      //println(pagecontent)
      val tokenL = aggregateTokensWord(pagecontent)
      //println("tokenL: " + tokenL)
      var num: Double = 0
      var MaximumLikelihood: scala.collection.mutable.MutableList[Double] = scala.collection.mutable.MutableList()
      for (m <- languageModels){
        var ml : Double = 0
        for (t <- tokenL) {
          num = m.tokenDistribution.applyOrElse(t._1, defaultDouble)
          //println(num + "\t" + t._1 + "\t" + t._2)
          //this seems to work better than using the log-likelihood
          if (num == 0) ml += 0 else ml += num * t._2
        }
        MaximumLikelihood += ml
      }
      //println( MaximumLikelihood )
      val l = MaximumLikelihood.zipWithIndex.maxBy(_._1)._2
      languageModels(l).name
    }

    def aggregateTokensWord(text : String) : scala.collection.mutable.Map[String, Int] = {
      val textClean : String = text.replaceAll( "[.,??\"!-;\\t\\n\\r\\f_]" , " " ).toLowerCase
      //println(textClean)
      val tokens = textClean.split("\\W+")
      val lCounts = tokens.groupBy(identity).mapValues(_.size)
      val lCountsMutable : scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()
      for(l <- lCounts){
        lCountsMutable.update( l._1, l._2 )
      }
      lCountsMutable
    }

    def explorePageForURL(url: URL): List[URL] = {
      try {
        val pagecontent = Source.fromURL(url)("UTF-8").mkString

        analyzeContent(pagecontent)

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
          lock.synchronized {
            if (new_url.getHost == host && !urls.contains(new_url.getPath)) {
              urls.put(new_url.getPath, new_url.getPath)
              queue.add(new_url)
            }
          }
        }
      }
    }
  }
}

