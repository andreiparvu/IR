/**
 * Created by mafu on 02.10.2015.
 */

import java.net.URL

import java.io.{BufferedWriter, OutputStreamWriter, FileOutputStream}
import scala.collection.mutable.Queue
import scala.io.Source
import java.nio.file.{Paths, Files}

case class url(path : URL, priority : Int)

class Crawler(seed : url){
  var frontier : Queue[url] = Queue(seed) //better: Priority queue instead of queue
  var backyard : Queue[url] = Queue()

  def hasNext() : Boolean = frontier.nonEmpty
  def getNext() : url = { val u = frontier.dequeue(); backyard += u; u}

  def explorePageForURLs(p : url) : List[url] = {
    println("explore...")
    println(p.path)
    try {
      val pagecontent = Source.fromURL(p.path)("UTF-8").mkString
      val testPattern = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
      val linksAsList = testPattern.findAllIn(pagecontent).toList.filter(s => s.contains("html"))
      val linksAsStr = linksAsList.map(s => s.replace("href=", "").replace("\"", "").replace("HREF=", ""))
      linksAsStr.map(s => url(new URL(p.path,s), 1))
    } catch {
      case e: java.io.IOException => List()
      case e: java.nio.charset.MalformedInputException => List()
      case e: java.io.FileNotFoundException => List()
    }
  }

  def status() : Unit = {
    println("STATUS")
    println("frontier - number of elements: " + frontier.length)
    println("frontier - next element: " + frontier.headOption)
    println("backyard - number of elements: " + backyard.length)
    println("backyard - number of elements: " + backyard.headOption)
  }
}

object Main {
  def main(args: Array[String]) {
    println("***First Crawler***")

    println("*Preprocessing") //Initialize
    //val seed : String = args(0) //this line might be activated
    val domain : String = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/" //this line might be dropped
    val seed : String =  "en.html"
    val domainURL : URL = new URL(domain+seed)
    val mycrawler : Crawler = new Crawler( url(domainURL, 1) )

    /*
    val st = "../weiterbildung/angebot/angebot-nach-faecher.html"
    val st2 = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/weiterbildung/angebot/angebot-nach-faecher.html"
    val turl = new URL( new URL(domain), st2)
    println(turl)
    mycrawler.frontier += new url( turl, 1)
    println(mycrawler.frontier)
    System.exit(1)
*/


    mycrawler.status()

    println("*Processing") //Crawling, on-the-fly analysis
    while(mycrawler.hasNext()){
      val nextpage : url = mycrawler.getNext()
      val links = mycrawler.explorePageForURLs(nextpage)
      for(l <- links){
        val u = new url(l.path, 1 )
        if( !mycrawler.frontier.contains(u) && !mycrawler.backyard.contains(u) && l.path.getHost == domainURL.getHost) mycrawler.frontier += u
      }
      //mycrawler.status()
    }

    println("*Postprocessing") //Analyze crawled pages
    mycrawler.status()
    mycrawler.backyard.foreach(println)

    val file = "backyard.txt"
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    for (x <- mycrawler.backyard) {
      writer.write(x + "\n")  // however you want to format it
    }
    writer.close()  }
}
