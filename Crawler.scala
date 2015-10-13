

/**
 * @author Matthias Fuhr, Andrei Parvu
 */

import scala.io.Source
import java.net.URL
import java.io._
import scala.collection.JavaConversions._
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

object Crawling {
  type MutList[T] = scala.collection.mutable.ListBuffer[T]
  type MutMap[X, Y] = scala.collection.mutable.HashMap[X, Y]

  var urls = new ConcurrentHashMap[String, String]
  var eliminated = new ConcurrentHashMap[Int, Int]().withDefaultValue(0)
  var content = new ConcurrentHashMap[Int, Int]().withDefaultValue(0)

  val studentOccurence = new AtomicInteger

  val hashes = new MutList[List[Int]]()

  def genPermutation(limit: Int): List[Int] = {
    val r = new scala.util.Random()

    r.shuffle(0 to limit toList)
  }

  def applyPermutation(l: List[Int], perm: List[Int]): List[Int] = {
    for (x <- perm) yield l(x)
  }

  // we can handle having more permutation and considering only a part of the
  // hash function, but we don't use that now
  val HASH_PREFIX = 32
  val NUM_TABLES = 1 // number of permutations
  val HASH_BITS = 32

  val perms = new MutList[List[Int]]()
  val tables = new MutList[scala.collection.mutable.Map[String, MutList[Int]]]()

  val NUM_SHINGLES = 7

  var host = ""

  def main(args: Array[String]) {
    val url = new URL(args(0))
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
        // We want to eliminate already discovered similar pages, so we don't count twice.
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
  }

  var queue = new java.util.concurrent.LinkedBlockingQueue[URL]

  val NUM_THREADS = 1

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
      threads += new Worker
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

    def analyzeContent(pagecontent: String, url: String) {
      content.update(pagecontent.hashCode(), 1 + content(pagecontent.hashCode()))

      var student = "(?i)student".r

      studentOccurence.addAndGet(student.findAllIn(pagecontent).size)

      // Keep only info from paragraphs.
      var paragraphs = "<p[^>]*>.*</p>".r

      var page_content = new MutList[String]()

      for (t <- paragraphs.findAllIn(pagecontent)) {
        // Eliminate tags.
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
    }

    def explorePageForURL(url: URL): List[URL] = {
      try {
        val pagecontent = Source.fromURL(url)("UTF-8").mkString

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
              urls.put(new_url.getPath, new_url.getPath)
              queue.add(new_url)
            }
          }
        }
      }
    }
  }
}