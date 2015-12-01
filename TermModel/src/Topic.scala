import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords
import scala.collection.mutable.Set

class Topic (topicMap: Map[String,String]) {
  val t_num = topicMap("num").toInt
  val t_title = Tokenizer.cleanWhiteSpaces(topicMap("title"))
  val t_dom = Tokenizer.cleanWhiteSpaces(topicMap("dom"))
  val t_desc = Tokenizer.cleanWhiteSpaces(topicMap("desc"))
  val t_smry = Tokenizer.cleanWhiteSpaces(topicMap("smry"))
  val t_narr = Tokenizer.cleanWhiteSpaces(topicMap("narr"))
  val t_con = Tokenizer.cleanWhiteSpaces(topicMap("con"))
  val t_fac = Tokenizer.cleanWhiteSpaces(topicMap("fac"))
  val t_def = Tokenizer.cleanWhiteSpaces(topicMap("def"))
  
  val qterms = buildVocabulary()
  val length = qterms.length
    
  def buildVocabulary(): List[String] = {
    var terms = List[String]()
    terms ++=StopWords.filter(Tokenizer.splitWords(t_dom).distinct)
    terms ++=StopWords.filter(Tokenizer.splitWords(t_title).distinct)
    terms ++=StopWords.filter(Tokenizer.splitWords(t_desc).distinct)
    terms ++=StopWords.filter(Tokenizer.splitWords(t_smry).distinct)
    terms ++=StopWords.filter(Tokenizer.splitWords(t_narr).distinct)
    terms ++=StopWords.filter(Tokenizer.splitWords(t_con).distinct)
    terms ++=StopWords.filter(Tokenizer.splitWords(t_def).distinct)
    return terms.filter(_.nonEmpty)
  }
  
  override def toString(): String = "Topic " + t_num + " " + t_title + "\n(" + qterms.mkString(", ") + ")"
}