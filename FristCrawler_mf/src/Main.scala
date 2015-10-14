/**
 * Created by mafu on 02.10.2015.
 */

import java.net.URL

import java.io._
import scala.collection.mutable.Queue
import scala.io.Source
import java.nio.file.{Paths, Files}

case class langModel(name : String, tokenDistribution : Map[String, Double])
case class url(path : URL, priority : Int, var lang : String)

class Crawler(seed : url, offline : Boolean, saveashtml : Boolean, langModel : List[langModel]){
  var frontier : Queue[url] = Queue(seed) //better: Priority queue instead of queue
  var backyard : Queue[url] = Queue()

  def hasNext() : Boolean = frontier.nonEmpty
  def getNext() : url = { val u = frontier.dequeue(); backyard += u; u}
  def defaultDouble = (s: String) => 0.toDouble

  def explorePageForURLs(p : url) : List[url] = {
    println("explore...")
    println(p.path.toString)
    //println(p.path.getPath)
    //println(p.path.hashCode)
    //println( Source.fromFile("html/" + p.path.hashCode + ".html").mkString )

    try {
      val pagecontent = if (offline) Source.fromFile("html/" + p.path.hashCode + ".html").mkString else Source.fromURL(p.path)("UTF-8").mkString
      val testPattern = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
      //val linksAsList = testPattern.findAllIn(pagecontent).toList.filter(s => s.contains("html"))
      val linksAsList = testPattern.findAllIn(pagecontent).toList.filter(s => s.endsWith(".html\""))
      val linksAsStr = linksAsList.map(s => s.replace("href=", "").replace("\"", "").replace("HREF=", ""))
      if(saveashtml && !offline) saveHTML(p.path.hashCode, pagecontent)
      val pagecontentOnlyParagraphs = extractTextFromHTML(pagecontent)
      p.lang = if (pagecontentOnlyParagraphs.isEmpty) "unknown" else classifyLanguage(pagecontentOnlyParagraphs, langModel)
      //println( pagecontentOnlyParagraphs )
      //println(lang)
      linksAsStr.map(s => url(new URL(p.path,s), 1, ""))
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

  def saveHTML(path : Int, pagecontent : String) : Unit = {
    val filename = "html/" + path + ".html"
    new PrintWriter(filename) { write(pagecontent); close }
    println("Printed: " + filename)
  }

  def extractTextFromHTML(pagecontent : String): String ={
    val patternParagarph = """(?s)<p>(.*?)p>""".r
    val patternLink = """(?s)<a(.*?)a>""".r
    //val patternParagarph = """(?s)<title>(.*?)title>""".r
    val ptext = patternParagarph.findAllIn(pagecontent).mkString.replaceAll("<p>","").replaceAll("</p>","").replaceAll("<br />","")
    patternLink.replaceAllIn(ptext,"")
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
}

object Main {
  def main(args: Array[String]) {
    println("***First Crawler***")

    println("*Preprocessing") //Initialize
    //val seed : String = args(0) //this line might be activated
    val domain : String = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/" //this line might be dropped
    val seed : String =  "en.html"
    val offline : Boolean = true
    val saveashtml : Boolean = false
    val domainURL : URL = new URL(domain+seed)

    val fileLangDE = "C:\\Users\\mafu\\Dropbox\\CAS ETH Software Engineering\\HS2015_InformationRetrieval\\Exercises\\Ex02\\languageModel_DE.txt"
    val fileLangEN = "C:\\Users\\mafu\\Dropbox\\CAS ETH Software Engineering\\HS2015_InformationRetrieval\\Exercises\\Ex02\\languageModel_EN.txt"
    val langModel_DE = new langModel( name = "german" , tokenDistribution = Source.fromFile(fileLangDE).getLines().toList.map(_.split("\t")).map{ case Array(k:String, v:String) => (k.toString, v.toDouble) }.toMap )
    val langModel_EN = new langModel( name = "english" , tokenDistribution = Source.fromFile(fileLangEN).getLines().toList.map(_.split("\t")).map{ case Array(k:String, v:String) => (k.toString, v.toDouble) }.toMap )
    val langModel = List(langModel_EN, langModel_DE)

    val mycrawler : Crawler = new Crawler( url(domainURL, 1, ""), offline, saveashtml, langModel )
    mycrawler.status()

    println("*Processing") //Crawling, on-the-fly analysis
    while(mycrawler.hasNext()){
      val nextpage : url = mycrawler.getNext()
      val links = mycrawler.explorePageForURLs(nextpage)
      for(l <- links){
        //val u = new url(l.path, 1, "")
        //if( !mycrawler.frontier.map(u => u.path).contains(l.path) && !mycrawler.backyard.contains(u) && l.path.getHost == domainURL.getHost) mycrawler.frontier += u
        if( !mycrawler.frontier.map(u => u.path).contains(l.path) && !mycrawler.backyard.map(u => u.path).contains(l.path) && l.path.getHost == domainURL.getHost) mycrawler.frontier += new url(l.path, 1, "")
      }
    }

    println("*Postprocessing") //Analyze crawled pages
    mycrawler.status()
    mycrawler.backyard.foreach(println)

    val file = "backyard.txt"
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    for (x <- mycrawler.backyard) {
      writer.write(x.path.toString + "\t" + x.path.hashCode + "\t" + x.lang + "\n")  // however you want to format it
    }
    writer.close()
  }
}
