package main.scala.termmodel

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.io.FileWriter

class ResultLogger(fname: String) {
  val rootpath = "src/main/resources/"
  val path = rootpath + fname
  var fw: FileWriter = null
  
  if (Files.exists(Paths.get(path))) {
    Files.delete(Paths.get(path))
  }
  
  Files.createFile(Paths.get(path))
  
  def append(text: String) {
    val fw = new FileWriter("test.txt", true)
    fw.write(text)
    fw.close()
  }

}