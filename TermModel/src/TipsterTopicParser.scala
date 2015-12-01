import scala.collection.mutable.Map
import scala.collection.mutable.MutableList
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

class TipsterTopicParser(var path: String) {
  var topics = new MutableList[Topic]()
  var topicIds = Map[Int, Int]()
  def getVocabulary(id: Int): String = {
    println(topicIds)
    return topics(topicIds(id)).qterms.mkString(" ")
  }
  def parse() {
    if (Files.exists(Paths.get(path))) {
      val doc = scala.io.Source.fromFile(path)
      var entryData = new StringBuilder()
      val anchors = List("num", "dom", "title", "desc", "smry", "narr", "con", "fac", "def")
      var entries = anchors.map(a => (a, "")).toMap[String, String]
      var currentTag = ""

      for (line <- doc.getLines()) {
        if (!line.matches("""\s*""")) {
          //println(line)
          if (line.startsWith("<top>")) {
            //Found new topic now looking for anchors
            //Head, num, dom, title, desc, smry, narr, con, fac, def
            //println("Found new topic")
          } else if (line.startsWith("<head>")) {
            // Content of this entry starts here
            entryData = new StringBuilder()

          } else if (line.startsWith("<num>")) {
            val numeric_regex = """\d+""".r
            val nb = -1

            if (line.matches(""".*\d+.*""")) {
              val nb = numeric_regex.findFirstIn(line).get.toInt
              entries += "num" -> nb.toString()
            }
            // Content of this entry starts here
            entryData = new StringBuilder()

          } else if (line.startsWith("<dom>")) {
            if (line.matches(""".*Domain:\s+.*""")) {
              entries += "dom" -> line.split("""Domain:\s+""")(1)
            }
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("<title>")) {
            if (line.matches(""".*Topic:\s+.*""")) {
              entryData.append(line.split("""Topic:\s+""")(1))
            }
            currentTag = "title"
          } else if (line.startsWith("<desc>")) {
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            currentTag = "desc"
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("<smry>")) {
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            currentTag = "smry"
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("<narr>")) {
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            currentTag = "narr"
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("<con>")) {
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            currentTag = "con"
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("<fac>")) {
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            currentTag = "fac"
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("<def>")) {
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            currentTag = "def"
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line.startsWith("</top>")) {
            // content of this entry ends here
            // so store content, and indicate that the entry is finished by 
            // setting data to null
            if (entryData.length > 0) {
              entries += currentTag -> entryData.toString()
            }
            val t = new Topic(entries)
            println(t)
            topicIds(t.t_num) = topics.size
            topics += t
            
            entries = anchors.map(a => (a, "")).toMap[String, String]
            // Content of this entry starts here
            entryData = new StringBuilder()
          } else if (line != null && line.length() > 0) {
            // we're in an entry as data is not null, so store the line
            if (currentTag == "con" || currentTag == "def") {
              if(line.matches("""^\d+\.\s+.*""")){
                //println(line.split("""^\d+\.\s+"""))
                val text = line.split("""^\d+\.\s+""")(1) + " "
                if (text.length() > 0) {
                  entryData.append(text)
                }
              }
              else {
                entryData.append(line + " ")
              }
            } else if (currentTag == "fac") {
              println("Ignoring factor...")
            } else {
              entryData.append(line + " ")
            }
          }
        }
      }
    }
  }
}