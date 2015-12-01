import scala.annotation.tailrec

/*
 * @author http://www.cis.upenn.edu/~matuszek/cis554-2012/Examples/Tokenizer.scala
 */
object AdvancedTokenizer {

  type Token = (TokenType, String)
  type Counts = scala.collection.mutable.Map[TokenType, Int]

  abstract class TokenType
  case object IntegerType extends TokenType
  case object FloatingPointType extends TokenType
  case object CharacterType extends TokenType
  case object StringType extends TokenType
  case object IdType extends TokenType
  case object KeywordType extends TokenType
  case object SymbolType extends TokenType
  case object CommentType extends TokenType
  case object DelimiterType extends TokenType
  case object EofType extends TokenType
  case object ErrorType extends TokenType

  val keywords = ("abstract case catch def do else extends false final " +
    "finally for forSome if implicit import lazy match " +
    "new null object override package private protected " +
    "requires return sealed super this throw trait try " +
    "true type val var while with yield _ : = => <- <: " +
    "<% >: # @ \u21D2 \u2190").split(' ').toSet

  val Eof = ((EofType, ""), "")

  val decimalNumeral = """0|[1-9][0-9]*"""
  val hexNumeral = "0[xX][0-9a-fA-F]+"
  val octalNumeral = "0[0-7]+"

  val exponentPart = """[Ee][+-]?\d+"""
  val floatType = """[FfDd]"""
  val fp1 = """\d+\.\d*(%s)?%s?""".format(exponentPart, floatType)
  val fp2 = """\.\d+(%s)?%s?""".format(exponentPart, floatType)
  val fp3 = """\d+(%s)%s?""".format(exponentPart, floatType)
  val fp4 = """\d+(%s)?%s""".format(exponentPart, floatType)

  val upper = """[A-Z$_]"""
  val lower = """[a-z]"""
  val letter = """[a-zA-Z$_]"""
  val op = """[!#$%&*+-/:<=>?@\^|~]+""" // XXX do I need to escape some chars?
  val idrest = """[a-zA-Z$_0-9]*(_%s)?""".format(op)
  val varid = lower + idrest
  val plainid = """(%s%s|%s|%s)""".format(upper, idrest, varid, op)
  val id = """(?s)(%s|`[^`]+`)(.*)""".format(plainid)

  val SkipWhitespace = """(?s)\s+(.*)""".r
  val IntegerLiteral = """(?s)(((%s)|%s|%s)([Ll]?))(.*)""".format(hexNumeral, octalNumeral, decimalNumeral).r
  val FloatingPointLiteral = "(?s)(%s|%s|%s|%s)(.*)".format(fp1, fp2, fp3, fp4).r
  val CharacterLiteral = """(?s)('.'|'\\[nbtfr'"\\]'|'\\[0-7]+'|'\\u[0-9a-fA-F]+')(.*)""".r
  val Id = id.r
  val Delimiter = """(?s)([;,.()\[\]{}])(.*)""".r
  
  //val StringLiteral = "(\"\"\".??\"\"\"|\"((\\\"|[^\"])*\"))(.*)".r
  val StringLiteral1 = "(?s)(\\\"\\\"\\\".*?\\\"\\\"\\\")(.*)".r
//  val StringLiteral2 = """(?s)(".*?")(.*)""".r 
  val StringLiteral2 = "(?s)(\"(\\\\.|.*?)*\")(.*)".r
      
      
  val SymbolLiteral = "(?s)'%s(.*)".format(plainid).r
//  val SingleLineComment = "(?s)(//($|[^\\n]*))(.*)".r

  def nextToken(text: String): (Token, String) = {
    if (text startsWith "//") {
      val endOfComment = text indexOf "\n"
      if (endOfComment == -1) ((CommentType, text), "")
      else ((CommentType, text.substring(0, endOfComment)), text.substring(endOfComment))
    } else if (text startsWith "/*") {
      val endOfComment = text indexOf "*/"
      if (endOfComment == -1) ((ErrorType, text), "")
      else ((CommentType, text.substring(0, endOfComment + 2)), text.substring(endOfComment + 2))
    } else
      text match {
        case "" => Eof
        case SkipWhitespace(rest) => nextToken(rest)
        case FloatingPointLiteral(token, _, _, _, _, rest) => ((FloatingPointType, token), rest)
        case IntegerLiteral(token, _, _, _, rest) => ((IntegerType, token), rest)
        case CharacterLiteral(token, rest) => ((CharacterType, token), rest)
        //    case SingleLineComment(token, _, rest) => ((CommentType, token), rest)
        case Id(token, _, _, _, rest) =>
          if (keywords contains token) ((KeywordType, token), rest)
          else ((IdType, token), rest)
        //     case Delimiter(token, rest) => ((KeywordType, token), rest)
        case SymbolLiteral(token, _, _, rest) => ((SymbolType, token), rest)
        case StringLiteral1(token, rest)      => ((StringType, token), rest)
        case StringLiteral2(token, _, rest)   => ((StringType, token), rest)
        case rest => {
          val firstChar = rest(0)
          if ("()[]{};,." contains firstChar) ((DelimiterType, firstChar.toString), rest.substring(1))
          else if ("\u21D2\u2190" contains firstChar) ((KeywordType, firstChar.toString), rest.substring(1))
          else ((ErrorType, rest), rest.substring(1))
        }
      }
  }

  def getAllTokens(source: String): List[Token] = {
    @tailrec def accumulateTokens(acc: List[Token], source: String): List[Token] =
      if (source == "") acc
      else {
        val (token, rest) = nextToken(source)
        accumulateTokens(token :: acc, rest)
      }
    accumulateTokens(List(), source)
  }



  def printCounts(counts: Counts) {
    println("---------- Counts ----------")
    for ((key, value) <- counts) println("%5d %s".format(value, key.toString))
    val commentCount = counts getOrElse(CommentType, 0)
    val howMany = counts.foldLeft (0) ((x, y) => x + y._2) - commentCount
    println("-----\n%5d TOTAL TOKENS (not counting the %d comments)".format(howMany, commentCount))
  }

}