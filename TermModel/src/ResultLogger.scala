import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.io.FileWriter

class ResultLogger(fname: String) {
  val rootpath = "src/resources/"
  val path = rootpath + fname
  var fw: FileWriter = null
  
  if (Files.exists(Paths.get(path))) {
    Files.delete(Paths.get(path))
  }
  
  val file = new File(path)
  
  def append(text: String) {
    val fw = new FileWriter(path, true)
    fw.write(text + "\n")
    fw.close()
  }

}

object ResultLogger {
  def main(args: Array[String]) {
    var r = new ResultLogger("test.txt")
    r.append("blabla")
    r.append("blabla")
  }
}