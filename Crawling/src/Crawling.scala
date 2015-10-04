

/**
 * @author andrei
 */

import scala.io.Source
import java.net.URL
import java.io._

import scala.collection.JavaConversions._

import java.util.concurrent._

object Crawling {
  var urls = new ConcurrentHashMap[String, String]
  var good_urls = new ConcurrentHashMap[String, String]
  var content = new ConcurrentHashMap[Int, Int]().withDefaultValue(0)
  
  val link = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
  var host = ""
  
  def main(args: Array[String]) {
    val url = new URL(link)
    host = url.getHost
    
    crawlPage(url)
    println(content.size + " " + good_urls.size)

    val pw = new PrintWriter(new File("data.txt" ))
    
    for (x <- urls) {
      pw.write(x._1 + "\n")
    }
    pw.flush
    pw.close
    println("Finished with " + good_urls.size + " good urls.")
    var sum = 0
    content.foreach{sum += _._2 - 1}
    println(sum + " duplicates")
  }

  var queue = new java.util.concurrent.LinkedBlockingQueue[URL]
  
  var lock = new Object
  
  def crawlPage(url: URL) {
    
    queue.add(url)
    urls.put(url.getPath, url.getPath)
    val a = scala.collection.mutable.ArrayBuffer[A]()

    a.append(new A(0))
    a.last.start()
    for (i <- 2 to 10) {
      a.append(new A(1))
      a.last.start()
    }

    a.foreach{_.join()}
    
    println("We have " + urls.size)
  }
  
  class A(p: Int) extends Thread {
    override def run() {
      var old = 0
      while (true) {
        try {
          val url = queue.poll(4, java.util.concurrent.TimeUnit.SECONDS)
  
          if (p == 0) {
            // just for debug
            println(urls.size + " " + queue.size)
          }
          
          if (url == null) {
            printf("Thread exit\n")
            return
          }
          
          val html = Source.fromURL(url).mkString
  
          content.update(html.hashCode(), 1 + content(html.hashCode()))
          
          good_urls.put(url.getPath, "")
          
          val links = """<a href="([^"]*)".*>""".r.findAllMatchIn(html)
      
          for (link <- links.map(link => link.group(1))) {
            try {
              val new_url = new URL(url, link)
              lock.synchronized {
                if (new_url.getHost == host && !urls.contains(new_url.getPath)) {
                  if (new_url.getPath.split('.').last == "html") {
                    urls.put(new_url.getPath, new_url.getPath)
                    queue.add(new_url)
                  }
                }
              }
            } catch {
              case e: java.net.MalformedURLException => {}
            }
          }
        } catch {
          case e: java.io.FileNotFoundException => {}
          case e: java.nio.charset.MalformedInputException => {}
        }
      }
    }
  }
}
