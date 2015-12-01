import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords
import scala.collection.mutable.Set

class Topic (topicMap: Map[String,String]) {
  val t_num = topicMap("num").toInt
  val t_title = Tokenizer.cleanWhiteSpaces(topicMap("title").toLowerCase)
  val t_dom = Tokenizer.cleanWhiteSpaces(topicMap("dom").toLowerCase)
  val t_desc = Tokenizer.cleanWhiteSpaces(topicMap("desc").toLowerCase)
  val t_smry = Tokenizer.cleanWhiteSpaces(topicMap("smry").toLowerCase)
  val t_narr = Tokenizer.cleanWhiteSpaces(topicMap("narr").toLowerCase)
  val t_con = Tokenizer.cleanWhiteSpaces(topicMap("con").toLowerCase)
  val t_fac = Tokenizer.cleanWhiteSpaces(topicMap("fac").toLowerCase)
  val t_def = Tokenizer.cleanWhiteSpaces(topicMap("def").toLowerCase)
  
  val qterms = buildVocabulary()
  val length = qterms.length
    
  def buildVocabulary(): List[String] = {
    var terms = List[String]()
    terms ++=StopWords.filter(Tokenizer.tokenize(t_dom).distinct)
    terms ++=StopWords.filter(Tokenizer.tokenize(t_title).distinct)
    terms ++=StopWords.filter(Tokenizer.tokenize(t_desc).distinct)
    terms ++=StopWords.filter(Tokenizer.tokenize(t_smry).distinct)
    terms ++=StopWords.filter(Tokenizer.tokenize(t_narr).distinct)
    terms ++=StopWords.filter(Tokenizer.tokenize(t_con).distinct)
    terms ++=StopWords.filter(Tokenizer.tokenize(t_def).distinct)
    return StopWords.filter(terms.filter(_.nonEmpty).toSeq).toList
  }
  
  override def toString(): String = "Topic " + t_num + " " + t_title + "\n(" + qterms.mkString(", ") + ")"
}