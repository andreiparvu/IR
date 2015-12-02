import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.StopWords
import scala.collection.mutable.Set

class Topic (topicMap: Map[String,String], tokenizer: Tokenizer) {
  val t_num = topicMap("num").toInt
  val t_title = tokenizer.cleanWhiteSpaces(topicMap("title").toLowerCase)
  val t_dom = tokenizer.cleanWhiteSpaces(topicMap("dom").toLowerCase)
  val t_desc = tokenizer.cleanWhiteSpaces(topicMap("desc").toLowerCase)
  val t_smry = tokenizer.cleanWhiteSpaces(topicMap("smry").toLowerCase)
  val t_narr = tokenizer.cleanWhiteSpaces(topicMap("narr").toLowerCase)
  val t_con = tokenizer.cleanWhiteSpaces(topicMap("con").toLowerCase)
  val t_fac = tokenizer.cleanWhiteSpaces(topicMap("fac").toLowerCase)
  val t_def = tokenizer.cleanWhiteSpaces(topicMap("def").toLowerCase)
  
  val qterms = buildVocabulary()
  val length = qterms.length
    
  def buildVocabulary(): List[String] = {
    var terms = List[String]()
    terms ++=StopWords.filter(tokenizer.tokenize(t_dom).distinct)
    terms ++=StopWords.filter(tokenizer.tokenize(t_title).distinct)
    terms ++=StopWords.filter(tokenizer.tokenize(t_desc).distinct)
    terms ++=StopWords.filter(tokenizer.tokenize(t_smry).distinct)
    terms ++=StopWords.filter(tokenizer.tokenize(t_narr).distinct)
    terms ++=StopWords.filter(tokenizer.tokenize(t_con).distinct)
    terms ++=StopWords.filter(tokenizer.tokenize(t_def).distinct)
    return StopWords.filter(terms.filter(_.nonEmpty).toSeq).toList
  }
  
  override def toString(): String = "Topic " + t_num + " " + t_title + "\n(" + qterms.mkString(", ") + ")"
}