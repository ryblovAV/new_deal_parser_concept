package com.alexk.parser

import java.io.{InputStream, InputStreamReader}
import java.nio.charset.Charset

import com.alexk.parser.JsonConfigReader.JsonFieldDefinition
import com.alexk.parser.legacy.XMLProductStreamProcessor
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.libs.json._
import play.api.libs.functional.syntax._
import ru.daron.deal_parser_concept.RawDeal

import scala.util.{Success, Try}
import scala.xml.{Node, NodeSeq}

object ParserType extends Enumeration {
  val XML = Value(1)
  val TEXT = Value(2)
  val CSV = Value(3)
  val JSON = Value(4)
}

object Currency extends Enumeration {
  val EUR = Value(49, "EUR")
  val USD = Value(148, "USD")
}

trait FieldExtractor[T] {
  def readField(fragment: T, fieldName: String): List[String]
}

class XmlFieldExtractor extends FieldExtractor[Node] {
  override def readField(fragment: Node, fieldName: String): List[String] = {
    val keyPath = fieldName.split('.')
    keyPath.foldLeft(fragment.asInstanceOf[NodeSeq])((current, key) => current \ key)
      .toList
      .map(_.text.trim)
  }
}

// TODO Option
case class CsvFieldDefinition(name: String, index: Int)

object CsvFieldExtractor {
  def fromNames(fieldNames: List[String], firstRow: Array[String]): CsvFieldExtractor = {
    val names = fieldNames.toSet
    val presentNames = firstRow.map(_.trim)

    val fieldIndex = names.map { name =>
      val index = presentNames.indexOf(name)
      index match {
        case -1 => throw new Exception(
          s"""Field "name" is not found in the header row.
             |If the feed doesn't contain a header row, you must specify field indices.
           """.stripMargin)
        case n => name -> n
      }
    }.toMap
    new CsvFieldExtractor(fieldIndex)
  }

  def fromIndices(fields: List[CsvFieldDefinition]): CsvFieldExtractor = {
    val fieldIndex = fields.map(f => f.name -> f.index).toMap
    new CsvFieldExtractor(fieldIndex)
  }
}

class CsvFieldExtractor(fieldIndex: Map[String, Int]) extends FieldExtractor[List[String]] {

  override def readField(fragment: List[String], fieldName: String): List[String] = {
    Option(fragment(fieldIndex(fieldName))).map(_.trim).getOrElse("") :: Nil
  }
}

//trait FieldParser {
//  protected def parseValue(value: List[String]): RawDeal => RawDeal
//  def acceptedType: Class[_]
//
//}

sealed trait ParserRule {
//  protected val ruleChain: Seq[ParserRule]
  def parseValue(value: List[String]): Option[_]// RawDeal => RawDeal
//  final def parse(value: List[String]): RawDeal => RawDeal =
//
}
//
//final case class IdParser() extends ParserRule {
//
//}

class UniversalDealParser[T](currency: Currency.Value,
                             fieldExtractor: FieldExtractor[T],
                             fields: Map[String, (String, ParserRule)]) {

  def parseDeal(in: T): Either[String, List[RawDeal]] = {
    try {
      val valuesList: List[(String, Option[_])] = fields
        .toList.map { case (field, (mongoField, rule)) =>
        val value = fieldExtractor.readField(in, field)
        val parsed = rule.parseValue(value)
        mongoField -> parsed
      }

      // merge by key???

      val values = valuesList.toMap

      Right(RawDeal.empty.copy(id = values("_id").get.asInstanceOf[String]) :: Nil)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }
}

trait FeedParser {
  val dealParser: UniversalDealParser[_]
  def parse(stream: InputStream): Iterator[RawDeal]

  // TODO close
}

object FeedParser {
  def apply(parserType: ParserType.Value,
            currency: Currency.Value,
            fields: Seq[(String, JsonFieldDefinition)]): FeedParser = {

    parserType match {
      case ParserType.XML =>
        val xmlDealParser = new UniversalDealParser[Node](currency, new XmlFieldExtractor(), Map.empty)

        val feedParser = ???
        null
      case ParserType.CSV =>

        new FeedParser {
          override val dealParser = new UniversalDealParser[List[String]](currency, CsvFieldExtractor.fromIndices(Nil), // TODO
            Map.empty) {

          }

          override def parse(stream: InputStream): Iterator[RawDeal] = {
            val reader = new InputStreamReader(stream, Charset.forName("utf-8"))
            val (quoteChar, delimiterChar, endOfLineSymbol) = ('"', '\t', "\n")
            val csvReader = new CsvListReader(reader, new CsvPreference.Builder(quoteChar, delimiterChar, endOfLineSymbol).build)
            import scala.collection.convert.wrapAsScala._

            val iterator: Iterator[List[String]] = new Iterator[java.util.List[String]] {
              override def hasNext: Boolean = true

              override def next(): java.util.List[String] = csvReader.read()
            }.takeWhile(_ != null).map(_.toList)

            iterator.map(line => dealParser.parseDeal(line)).map(_.right.get).flatten
          }
        }
    }
  }
}

object JsonConfigReader {
  case class JsonConfiguration(feedFormat: String, currency: String, data: JsObject)
  case class JsonFieldDefinition(mongoField: String, rules: Map[String, Either[String, List[String]]])

  private implicit val configurationReads: Reads[JsonConfiguration] = (
    (JsPath \ "feed_format").read[String] and
      (JsPath \ "currency").read[String] and
      (JsPath \ "data").read[JsObject]
    ) (JsonConfiguration.apply _)

  private def readConfig(jsonString: String): JsonConfiguration = Json.parse(jsonString).validate[JsonConfiguration] match {
    case s: JsSuccess[JsonConfiguration] => s.get
    case e: JsError => throw new Exception(s"Can't read JSON parser configuration: $e")
  }

  private def parseFieldDefinition(fieldName: String, json: JsValue): JsonFieldDefinition = {
    val rules = json.as[JsObject].value.map { case (k, v) => k -> (v match {
      case s: JsString => Left(s.value.trim)
      case b: JsBoolean => Left(b.toString)
      case n: JsNumber => Left(n.toString)
      case a: JsArray => Right(a.value.map(_.as[String].trim).toList)
      case _ => throw new Exception(s"""No value specified for field "$fieldName", rule "$k"""")
    })
    }.toMap

    val mongoField = Try(rules("db_field").left.get).getOrElse(
      throw new Exception(s""""db_field" must be specified for field "$fieldName""""))

    JsonFieldDefinition(mongoField, rules - "db_field")
  }

  def createParser(jsonString: String): FeedParser = {
    val config: JsonConfiguration = readConfig(jsonString)

    val parserType = ParserType.withName(config.feedFormat)
    val currency = Currency.withName(config.currency)

    val fieldDefinitions = config.data.fields.flatMap { case (name, jsValue) =>
      jsValue.validate[JsArray] match {
        case s: JsSuccess[JsArray] => s.get.value.map(name -> parseFieldDefinition(name, _))
        case e: JsError => throw new Exception(s"Field definition must be an array: $e")
      }
    }

    FeedParser(parserType, currency, fieldDefinitions)
  }
}