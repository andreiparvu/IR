

/**
 * @author andrei
 */

import scala.io.Source
import java.net.URL

object Crawling {
  var urls = collection.mutable.Set[String]()
  val link = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
  var host = ""
  
  def main(args: Array[String]) {
    val url = new URL(link)
    host = url.getHost
    
    crawlPage(url)
  }
  
  def crawlPage(url: URL) {
    var queue = new scala.collection.mutable.Queue[URL]
    queue += url
    urls += url.getPath
    
    while (!queue.isEmpty) {
      try {
        val url_path = queue.dequeue.getPath
      
        val html = Source.fromURL(url).mkString
    
        urls += url_path
        
        val links = """<a href="([^"]*)".*>""".r.findAllMatchIn(html)
    
        for (link1 <- links) {
          var link = link1.group(1)
          var new_url = new URL(url, link)
          
          if (new_url.getHost == host && !urls.contains(new_url.getPath)) {
            urls += new_url.getPath
            queue += new_url
          }
        }
      } catch {
        case e: java.io.FileNotFoundException => {}
        case e: java.nio.charset.MalformedInputException => {}
      }
    }
    
    println("We have " + urls.size)
  }
}