package com.alexk.parser.legacy

import java.lang.StringBuilder
import java.net.{URLDecoder, URLEncoder}
import java.text.Normalizer
import java.util.regex.{Matcher, Pattern}

import org.apache.commons.lang3.{StringEscapeUtils, StringUtils => CommonStringUtils}

import scala.util.control.Exception._
import scala.xml.{Group, NodeSeq, Text}

//scalastyle:off number.of.methods
object StringUtils {
  val ENGLISH_STOP_WORDS = Set(
    "a", "an", "and", "are", "as", "at", "be", "but", "by",
    "for", "if", "in", "into", "is", "it",
    "no", "not", "of", "on", "or", "such",
    "that", "the", "their", "then", "there", "these",
    "they", "this", "to", "was", "will", "with", "&")

  val START_TOKEN = "<qstart>"
  val END_TOKEN = "<qend>"

  case class UrlStrippedText(rawText: String, strippedText: String, urls: List[TextRegion])
  case class Region(start: Int, end: Int)

  object TextRegion {

    def apply(s: String): TextRegion = TextRegion(s, Region(0, s.length), s)
    def apply(s: String, region: Region): TextRegion = TextRegion(s, region, s)
    def apply(s: String, source: String): TextRegion = TextRegion(s, Region(0, s.length), source)

    def empty: TextRegion = TextRegion("")
  }

  case class TextRegion(text: String, region: Region, source: String) {

    def this(s: String) = this(s, Region(0, s.length), s)
    def this(s: String, source: String) = this(s, Region(0, s.length), source)

    def startFrom(baseIndex: Int): TextRegion = this.copy(region = Region(region.start + baseIndex, region.end + baseIndex))

    def within(region: Region): TextRegion = startFrom(region.start)
    def within(textRegion: TextRegion): TextRegion = startFrom(textRegion.region.start)

    val length: Int = text.length
    val isEmpty: Boolean = text.isEmpty
    val nonEmpty = !isEmpty

    lazy val trim: TextRegion = "^\\s*(.*[^\\s])\\s*$".r
      .findAllIn(text).matchData.toList.headOption
      .flatMap(m => subRegion(m.start(1), m.end(1)))
      .getOrElse(this)

    def subRegion(start: Int, end: Int): Option[TextRegion] = {
      if(start < end) {
        val substring = if (end < text.length) text.substring(start, end) else text.substring(start)

        //here is the tricky part if the region text can't fit the dimensions make subregion with old dimensions
        if (end + region.start > source.length || substring != source.substring(start + region.start, end + region.start)) {
          Some(TextRegion(substring, region, source))
        } else {
          Some(TextRegion(substring, Region(start, end), source).within(region))
        }
      } else {
        None
      }
    }

    def subRegion(start: Int): Option[TextRegion] = subRegion(start, text.length)

    def withText(newText: String): TextRegion = this.copy(text = newText)
    def addText(newText: String): TextRegion = TextRegion(text + newText, Region(region.start, region.end + newText.length), source)

    /**
     * Creates new TextRegion with source string as result of concatenation of first text separator and second text
     * @param separator string to put in between
     * @param that TextRegion to combine with
     */
    def combineWith(separator: String)(that: TextRegion): TextRegion = TextRegion( (text + separator + that.text).trim )

    def + (s: String): TextRegion = addText(s)
    def + (that: TextRegion): TextRegion = {
      val newText = text + that.text
      val end = (region.start + newText.length) max that.region.end
      TextRegion(newText, Region(region.start, end), source)
    }

  }

  // Returns a pattern where all punctuation characters are escaped.
  lazy val SPECIAL_CHARS = Pattern.compile("(\\W)")
  //  lazy val TOKENIZER = Pattern.compile("([\\<\\>\\|\\[\\]\\^\\{\\}\\\\–\\s]|-{2,})+")
  lazy val TOKENIZER = Pattern.compile("([\\p{Punct}&&[^-'\\&\\+#]]|\\s|”|“)+|(-|\\&|'|#){2,}|\\+{3,}")
  lazy val POSSESSIVES = Pattern.compile("'s$")
  lazy val EDGE_PUNCTUATION = Pattern.compile("^[\\p{Punct}\\p{InGeneralPunctuation}\\pP]+|[\\p{Punct}\\p{InGeneralPunctuation}\\pP&&[^\\+#]]+$")
  lazy val ARTICLES = Pattern.compile("(^|\\s)+(an?|the)(\\s|$)+")
  lazy val ALL_PUNCTUATION = Pattern.compile("\\p{P}+")
  lazy val PUNCTUATION_BUT_DASHES = Pattern.compile("[\\p{P}&&[^-]]+")
  lazy val WHITE_SPACE = Pattern.compile("\\s+")
  lazy val WORD_CHAR = Pattern.compile("[a-z]")
  lazy val UPPERCASED_STRING = Pattern.compile("^\\p{javaUpperCase}+\\p{javaLowerCase}+")
  lazy val TAB = Pattern.compile("\t")
  lazy val EXCESS_PUNCT = Pattern.compile("[^\\^]\\p{P}{3,3}(\\p{P}+)")
  lazy val TWITTER_REPLY: Pattern = Pattern.compile("^(\\.?@[a-z0-9_]{1,20} )+")
  lazy val ALL_DIGITS: Pattern = Pattern.compile("\\d+")
  lazy val ESCAPED_UNICODE = Pattern.compile("(\\\\u([a-f]|[0-9]){4,4})", Pattern.CASE_INSENSITIVE)
  lazy val ESCAPED_SPECIAL_CHARS = Pattern.compile("(\\\\(b|f|n|r|t|\"|/|\\\\))", Pattern.CASE_INSENSITIVE)
  lazy val TRUNCATED_HTML_ENTITY = Pattern.compile("\\s*\\&[^; ]*$")
  lazy val CONTENT_CHARSET = Pattern.compile(".* charset=\"?([\\w-]{5,})\"?$")
  lazy val HTTP_PROTOCOL = Pattern.compile("^https?://")
  lazy val EMAIL = Pattern.compile("""[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+(?:[A-Z]{2}|com|org|net|edu|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)\b""") //scalastyle:ignore
  lazy val MiddleInitial = Pattern.compile("(\\S{2,})( [a-zA-Z]\\.?)+$")
  lazy val APOSTROPHES = Pattern.compile("[′’ʼʻ՚´]") // unicode version of apostrophes, see http://en.wikipedia.org/wiki/Apostrophe
  lazy val ENTITY_PARENS = Pattern.compile(" \\(.+")
  lazy val DASHES = Pattern.compile("[‒–]") // unicode version of dashes, http://en.wikipedia.org/wiki/Dash
  private lazy val DIACRITICALS = Pattern.compile("[\\p{InCombiningDiacriticalMarks}&&[^\u0303]]+")

  def stripMiddleInitial(name: String): String = {
    val m = MiddleInitial.matcher(name)
    if (m.matches) {
      m.group(1)
    } else {
      name
    }
  }

  def normalizeApos(str: String): String = APOSTROPHES.matcher(str).replaceAll("'")
  def normalizeDash(str: String): String = DASHES.matcher(str).replaceAll("-")
  lazy val URL_PREFIX = Pattern.compile("(https?://)?(www\\.)?", Pattern.CASE_INSENSITIVE)

  // Used to strip broken urls. The first case is for broken urls at the end of re-tweets and the second case
  // captures cases where the URL detector fails, namely cases with invalid characters preceding urls such as
  // "...http://blah.com". Unfortunately, we're currently not detecting things like ".www.google.com"
  lazy val BROKEN_URLS: Pattern =  Pattern.compile(" http[^ ]* \\.{3,}$|https?://[^\\s]+")
  lazy val UNDERSCORE = java.util.regex.Pattern.compile("_")

  def escapeRegEx(str: String) : String = SPECIAL_CHARS.matcher(str).replaceAll("\\\\$1")

  // Note: use this method when trying to escape html entities and displaying text via web app. StringEscapeUtils
  // library from apache commons will convert apostrophes to &apos; and Lift and/or some
  // web browsers don't support that entity. Therefore, it's best to instead replace apostrophes with
  // &#39; Unit tests should cover this. This will also convert line breaks to proper html tag.
  def escapeHtmlEntities(str: String) : String = {
    StringEscapeUtils.escapeXml(str).replaceAll("\n", "<br />").replaceAll("&apos;", "&#39;")
  }

  // Note: this def is used for parse the DafitiXml in the first trial
  def escapeXMLEntities(str: String) : String = {
    StringEscapeUtils.escapeXml(str)
  }

  def unescapeXMLEntities(str: String) : String = {
    StringEscapeUtils.unescapeXml(str)
  }

  def isAllDigits(s: String): Boolean = ALL_DIGITS.matcher(s).matches()

  def isBadToken(t: String): Boolean = t.isEmpty || !WORD_CHAR.matcher(t).find()

  def unescapeHtmlEntities(str: String): String =  {
    StringEscapeUtils.unescapeHtml4(str)
  }

  def decodeUrl(url: String): String =
    URLDecoder.decode(url, "UTF-8")

  def encodeUrl(url: String): String =
    URLEncoder.encode(url, "UTF-8")

  // strips control characters -- they're useless and cause json parsing issues
  def stripControlChars(str: String): String = {
    val sb: StringBuffer = new StringBuffer()
    for (c <- str) {
      if (c == '\t' || c == '\n' || c > '\u001f') {
        sb.append(c)
      } // else don't add it
    }
    sb.toString
  }

  def trimUrlPrefix(url: String): String = {
    stripEdgePunct(URL_PREFIX.matcher(url).replaceAll(""))
  }

  def stripPossessives(str: String): String = POSSESSIVES.matcher(str).replaceAll("")

  def stripEdgePunct(str: String): String = EDGE_PUNCTUATION.matcher(str).replaceAll("")

  // this is an aggressive tokenizer. as of comment writing, it splits at everything except &,' and -.
  def tokenize(str: String): Array[String] = TOKENIZER.split(str).map(stripEdgePunct(_)).filter(!_.isEmpty)

  // Note: initially, at least, used to strip the first mention in a tweet of training data for classification
  // models. Reasoning: if someone addresses @aplusk (we'll extract Ashton Kutcher) and the remainder of the tweet
  // may be used in an ngram classifier even though it may not be related to movies/tv (the current category he's in)
  // thereby introducing noisy training data.
  def stripTweetAddressee(str: String): String = TWITTER_REPLY.matcher(str).replaceFirst("")

  def subRegion(str: String, subStr: String, end: Int = 0): Region = {
    val startIndex = str.indexOf(subStr,end)
    if (startIndex >= 0) {
      Region(startIndex, startIndex + subStr.length)
    } else {
      Region(0,0)
    }
  }

  def stripUnderscores(s: String): String = UNDERSCORE.matcher(s).replaceAll(" ")

  def snipText(str: String, maxLen: Int): String = {
    if (str.length <= maxLen) {
      str
    } else {
      CommonStringUtils.substringBeforeLast(str.substring(0,maxLen-2)," ") + "..."
    }
  }

  def stripRegions(str: String, textStart: Int, regions: List[Region], builder: StringBuilder) : StringBuilder = {
    regions match {
      case Nil =>
        builder append (str.substring(textStart))
      case r :: restOfRegions =>
        stripRegions(str, r.end, restOfRegions, builder append str.substring(textStart,r.start))
    }
  }

  def stripExcessPunctuation(text: String): String = {
    def helper(m: Matcher, badList: List[Region]): List[Region] = {
      if (m.find()) {
        helper(m, badList :+ Region(m.start(1),m.end(1)))
      } else {
        badList
      }
    }

    stripRegions(text, 0,helper(EXCESS_PUNCT.matcher(text),List.empty[Region]), new StringBuilder).toString
  }

  def extractRegexRegions(source: String)(m: Matcher, groups: List[TextRegion], groupIndex: Int): List[TextRegion] = {
    if (m.find()) {
      extractRegexRegions(source)(m, groups :+ TextRegion(m.group(groupIndex), Region(m.start(groupIndex), m.end(groupIndex)), source), groupIndex)
    } else {
      groups
    }
  }

  def recalculateRegions(text: String, regionedTexts: List[(String, Region)]): List[(String, Region)] = {
    regionedTexts.flatMap({
      case (rText, Region(start, end)) =>
        val index = text.indexOf(rText, start)

        if (index != -1) (rText, Region(index, index + rText.length())) :: Nil else Nil
    })
  }

  def extractEscapedUnicode(text: String): List[TextRegion] = {
    extractRegexRegions(text)(ESCAPED_UNICODE.matcher(text), Nil, 1)
  }

  // ---  TEMPORARY -- to undo json.simple stuff ---
  def extractEscapedChars(text: String): List[TextRegion] = {
    extractRegexRegions(text)(ESCAPED_SPECIAL_CHARS.matcher(text), Nil, 1)
  }

  def stripPunctuation(str: String): String = ALL_PUNCTUATION.matcher(str).replaceAll("")

  // filesystemName: strips out all punctuation and separates tokens with underscores
  // e.g. "Condiments (Oil / Vinegar / etc.)" ==> "Condiments_Oil_Vinegar_etc"
  //      "Skin Care - Sun & Tanning (Bulk Retail)" ==> "Skin_Care_Sun_Tanning_Bulk_Retail"
  def filesystemName (str:String): String = TOKENIZER.split(str).map(stripEdgePunct(_)).filter(!_.isEmpty).mkString("_")

  def stripPunctuationButDashes(str: String): String = PUNCTUATION_BUT_DASHES.matcher(str).replaceAll("")

  def stripArticles(str: String): String = ARTICLES.matcher(str).replaceAll(" ").trim()

  def prepareForTokenization(str: String): String = stripArticles(stripPunctuation(str))

  //  def breakStringIntoWords(str: String) : List[String] = stripPunctuation(str).split("\\s+").toList

  def stripTruncatedHtmlEntity(str: String): String = TRUNCATED_HTML_ENTITY.matcher(str).replaceAll("")

  def truncateString(text: String, length: Int, wholeWord: Boolean = true) : String = {
    if ( text.length > length ) {
      val ellipsis = "..."
      val tempLen = length - ellipsis.length

      // if whole words are required, truncate prior to last word
      if (wholeWord) {
        val effectiveLen = if (!text(tempLen).isWhitespace) {
          text.substring(0, tempLen).lastIndexWhere(c => c.isWhitespace) match {
            case -1 => return "" //scalastyle:off
            case l => l
          }
        } else {
          tempLen
        }

        text.substring(0, effectiveLen) + ellipsis

        // make sure truncation isn't done in the middle of an html entity, otherwise will yeild invalid xml
      } else {
        stripTruncatedHtmlEntity(text.substring(0, tempLen)) + ellipsis
      }
    } else {
      text
    }
  }

  def removeEnclosedSection(text: String, startSeparator: String, endSeparator: String) : String = {
    val splitByStart = text.split(startSeparator)
    splitByStart.foldLeft(splitByStart(0))((soFar, seg) => {
      val segments = seg.split(endSeparator)
      soFar + {if (segments.size > 1) segments.last else ""}
    })
  }

  // this method will convert all line breaks into <br/> tags
  def formatUserTextAreaInputAsHtml(input : String) : NodeSeq = {
    (for (line <- input.split('\n')) yield {
      Group(Text(line) ++ <br/>)
    }) toList
  }

  def stripProtocol(url: String): String = HTTP_PROTOCOL.matcher(url).replaceAll("")

  def toIntOpt(s: String) = catching(classOf[NumberFormatException]) opt s.toInt

  /**
   * Rewriting this because the original in StringHelpers doesn't respect other chars other than '_' or ' '
   * Capitalize every "word" in the string. A word is either separated by spaces or underscores.
   * @param in string to capify
   * @return the capified string
   */
  def capify(in: String): String = {
    val tmp = ((in match {
      case null => "" //scalastyle:off
      case s => s
    }).trim match {
      case "" => "n/a"
      case s => s
    })
    val sb = new StringBuilder
    capify(tmp, 0, false, sb)
    sb.toString
  }

  private def capify(in: String, pos: Int, lastLetter: Boolean, out: StringBuilder): Unit = {
    if (pos >= in.length) return  //scalastyle:off
    else {
      in.charAt(pos) match {
        case c if Character.isDigit(c) => out.append(c); capify(in, pos + 1, false, out)
        case c if Character.isLetter(c) => out.append(if (lastLetter) c else Character.toUpperCase(c)) ; capify(in, pos + 1, true, out)
        case c =>
          out.append(c)
          // if c is an apostrophe, then we should treat it as if the last was a letter, because it is going to be part
          // of the same word
          capify(in, pos + 1, c == '\'', out)
      }
    }
  }

  def removeRetweetText(tweet: String) : String = {
    val endClause = ": "
    (tweet.indexOf("RT @"), tweet.indexOf(endClause))  match {
      case (0, end) if (end != -1) => tweet.substring(end + endClause.length())
      case _ => tweet
    }
  }

  def getContentEncoding(contentType: String): Option[String] = {
    val m = CONTENT_CHARSET.matcher(contentType.toLowerCase)
    if (m.find()) {
      Some(m.group(1))
    } else {
      None
    }
  }

  def normalize(str: String): List[String] = {
    val lowerCased = str.toLowerCase.trim
    val rawTokens = lowerCased.split(" ").toList

    lowerCased :: rawTokens.map(_.trim).filterNot(t => t.isEmpty || ENGLISH_STOP_WORDS.contains(t))
  }

  /**
   * Strips all diacritical marks (ie, converts á to a) except the tilde in ñ
   * @param str
   * @return
   */
  def stripDiacriticalMarks(str: String): String = {
    val nfdNormalizedString = Normalizer.normalize(str, Normalizer.Form.NFD)
    Normalizer.normalize(DIACRITICALS.matcher(nfdNormalizedString).replaceAll(""), Normalizer.Form.NFC)
  }

  /**
   * Checks if the string starts with the Capital letter and contains lower case letters
   * @param s input string
   * @return upper cased true or false
   */
  def upperCased_?(s: String): Boolean = UPPERCASED_STRING.matcher(s).find()


  def levenshteinDistance(str1: String, str2: String): Int = {
    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1

      d(i)(j) = List(
        d(i - 1)(j) + 1, // deletion
        d(i)(j - 1) + 1, // insertion
        d(i - 1)(j - 1) + cost // substitution
      ).min
    }

    d(lenStr1)(lenStr2)
  }

  /**
   * Will be used in toString methods to truncate output for long listData
   *
   * List(a,b,c,d)  => List(a,b,c, ... more [1 items])
   *
   * @param data
   * @param limit elements to display
   * @tparam T
   * @return truncated string representation
   */
  def limitedStringRepr[T](data: Traversable[T], limit: Int ): String = {
    if (data.size <= limit) {
      data.toString()
    } else {
      val str = data.take(limit).toString() // List(a,b,c)
      str.substring(0, str.length - 1) + s", ... more [${data.size - limit} values])" // List(a,b,c, ... more [10 items])
    }
  }

  /**
   * Will be used in toString methods. Will limit long value to specified length
   *
   * @param data
   * @param maxValueLength
   * @tparam T
   * @return Map[T, truncated value]
   */
  def limitMapValueLength[T](data: Map[T, String], maxValueLength: Int): Map[T, String] = {
    data.map{
      case (name, value) if value.length >  maxValueLength => name -> (value.substring(0, maxValueLength) + s" ... [${value.length -maxValueLength}] characters more")
      case (name, value) => name -> value
    }
  }
}
