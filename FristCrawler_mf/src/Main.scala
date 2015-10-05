/**
 * Created by mafu on 02.10.2015.
 */

import java.net.URL

import scala.collection.mutable.Queue
import scala.io.Source
import java.nio.file.{Paths, Files}

case class url(href : String, path : URL, priority : Int)

class Crawler(seed : url){
  var frontier : Queue[url] = Queue(seed) //better: Priority queue instead of queue
  var backyard : Queue[url] = Queue()

  def hasNext() : Boolean = frontier.nonEmpty
  def getNext() : url = { val u = frontier.dequeue(); backyard += u; u}

  def explorePageForURLs(p : url) : List[url] = {
    println("explore...")
    println(p.path)
    val seed = new URL(p.path.toString.substring(0, p.path.toString.lastIndexOf("/")) )
    try {
      val pagecontent = Source.fromURL(p.path)("UTF-8").mkString
      val testPattern = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
      val linksAsList = testPattern.findAllIn(pagecontent).toList.filter(s => s.contains("html"))
      val linksAsStr = linksAsList.map(s => s.replace("href=", "").replace("\"", "").replace("HREF=", ""))
      linksAsStr.map(s => url(s, new URL(seed,s), 1))
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
    val mycrawler : Crawler = new Crawler( url(domain+seed, new URL(domain+seed), 1) )

    mycrawler.status()

    println("*Processing") //Crawling, on-the-fly analysis
    while(mycrawler.hasNext()){
      val nextpage : url = mycrawler.getNext()
      val links = mycrawler.explorePageForURLs(nextpage)
      for(l <- links){
        val u = new url( domain+l.href.replace(" ",""), new URL(domain+l.href.replace(" ","")), 1 )
        if( !mycrawler.frontier.contains(u) && !mycrawler.backyard.contains(u) ) mycrawler.frontier += u
      }
      mycrawler.status()
    }

    println("*Postprocessing") //Analyze crawled pages
    mycrawler.status()
    mycrawler.backyard.foreach(println)
    println( mycrawler.backyard.length )
  }
}
