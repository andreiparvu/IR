package ch.ethz.dal.tinyir.processing

import com.github.aztek.porterstemmer.PorterStemmer

object Tokenizer {
  val whitespace_regex = """\s+""".r
  //val punct_regex = """[\p{P}\p{S}]""".r
  val punct_regex = """[^\P{P}-]+""".r
  val useless_punct_regex = """[\(\)\[\],\.\?\!:;\\_\"&<>]""".r
  //val punct_regex = """[\p{P}]""".r
  val digit_regex = """\d+""".r
  val start_whitespace_regex = """^\s+""".r
  val trailing_whitespace_regex = """\s+$""".r
  val r1 = """(?<=\w)-\s*\n\s*""".r
  val r2 = """(?<=\W)(\p{P})(?! )""".r
  val r3 = """(?! )(\p{P})(?=\W)""".r
  val punct = Set(".", "?", "!", ":", ";", "-", "_", "(", ")", "[", "]", "...", "\"", "\\", "/", ",")
    
  def tokenize (text: String) : List[String] = {
    //StopWords.filter(text.split("[ .,;:?!\t\n\r\f]+")).toList.map(w => PorterStemmer.stem(w))
    StopWords.filter(text.split("\\s+|(?=\\W\\p{Punct}|\\p{Punct}\\W)|(?<=\\W\\p{Punct}|\\p{Punct}\\W})")).toList.map(w => PorterStemmer.stem(w))
  }
    
  def splitWords(text: String): List[String] = {
    //Lower case
    var cleanText = text.toLowerCase()
    cleanText = digit_regex.replaceAllIn(cleanText, "")
    cleanText = useless_punct_regex.replaceAllIn(cleanText, "")
    cleanText = whitespace_regex.replaceAllIn(cleanText, " ")
    //Removing punctuation
    cleanText = punct_regex.replaceAllIn(cleanText, "")
    cleanText = r1.replaceAllIn(cleanText, "")
    cleanText = r2.replaceAllIn(cleanText, "$1 ")
    cleanText = r3.replaceAllIn(cleanText, " $1")
    cleanText = start_whitespace_regex.replaceAllIn(cleanText, "")
    cleanText = trailing_whitespace_regex.replaceAllIn(cleanText, "")
    //Splitting to words and removing stop words
    return StopWords.filter(whitespace_regex.split(cleanText).filter(w => !punct.contains(w)).filter(_.nonEmpty)).map(w => PorterStemmer.stem(w)).toList
  }
  
  def getTokens(text: String): List[String] = {
    var items = text.split("[\\p{Punct}\\s]+")
    //var items = text.replaceAll("[,.!?;:]", "$0 ").toLowerCase().replaceAll("\\d"," ").replaceAll("\\p{Punct}", " ").replaceAll("\\s+", " ").split("\\s+")
    return StopWords.filter(items.toList.filter(w => !punct.contains(w) && w.length() > 1).filter(_.nonEmpty).map(w => PorterStemmer.stem(w))).toList
  }
  
  def cleanWhiteSpaces(text: String): String = {
    val whitespace_regex = """\s+""".r
    whitespace_regex.replaceAllIn(text, " ")
  }
}