package com.alexk.parser

import com.alexk.parser.FieldParserDefinition.FieldRules
import ru.daron.deal_parser_concept._

import scala.util.Try
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

  def fromIndices(fields: Seq[CsvFieldDefinition]): CsvFieldExtractor = {
    val fieldIndex = fields.map(f => f.name -> f.index).toMap
    new CsvFieldExtractor(fieldIndex)
  }
}

class CsvFieldExtractor(fieldIndex: Map[String, Int]) extends FieldExtractor[List[String]] {

  override def readField(fragment: List[String], fieldName: String): List[String] = {
    Option(fragment(fieldIndex(fieldName))).map(_.trim).getOrElse("") :: Nil
  }
}

trait ParserRule[T] {
  def handle(value: String): T
}

object FieldParserFactory {
  def apply(field: FieldParserDefinition): FieldParser[_] = {
    field.mongoField match {
      case "_id" => new StringFieldParser(field)
      case "t" => new StringFieldParser(field)
      case "at" => new StringFieldParser(field)
      case "tt" => new TokensParser(field)
      case "tags" => new ArrayParser[Tag](field)(Tag)
      case "info.cf" => new AddInfoListFieldParser(field)
      case "info.bf" => new AddInfoBooleanFieldParser(field)
      case "ia" => new BooleanFieldParser(field)
    }
  }
}

class UniversalDealParser[T](currency: Currency.Value,
                             fieldExtractor: FieldExtractor[T],
                             val fields: Seq[(String, String, FieldParser[_])]) {

  def parseDeal(in: T): Try[List[RawDeal]] = {
    Try {
      val valuesList: Seq[(String, Any)] = fields
        .toList.flatMap { case (field, mongoField, fieldParser) =>
        val value = fieldExtractor.readField(in, field)
        val parsed = if (fieldParser != null) fieldParser.parseValue(value) else None // TODO remove NULL check
        parsed.map(mongoField -> _)
      }

      // merge by key
      val values: Map[String, Any] = valuesList.groupBy(_._1).map { case (key, list) =>
          val definedValues = list.map(_._2)
          if (definedValues.size < 2)
            list.head
          else {
            definedValues.head match {
              case _: Seq[_] => key -> Some(definedValues.flatMap(_.asInstanceOf[Seq[_]]))
              case _: RawTokens => key -> definedValues.map(_.asInstanceOf[RawTokens])
                .foldLeft(RawTokens.empty) { case (rt, t) => rt.merge(t) }
              case _ => throw new Exception(s"Multiple values found for a field that allows a single value only: $key")
            }
          }
      }

      val addInfo = AddInfo.empty.copy(
        booleanFields = values.get("info.bf").map(_.asInstanceOf[Map[String, Boolean]]).getOrElse(Map.empty),
        listFields = values.get("info.cf").map(_.asInstanceOf[Map[String, List[String]]]).getOrElse(Map.empty)
      )

//      def readStringOpt(name: String): Option[String] = values.get(name).flatMap(_.asInstanceOf[Option[String]])

      RawDeal.empty.copy(
        id = values("_id").asInstanceOf[String],
        title = values.get("t").map(_.asInstanceOf[String]),
        description = values.get("at").map(_.asInstanceOf[String]),
        isActive = values.get("is").map(_.asInstanceOf[Boolean]).getOrElse(false), // should be true really
        rawTokens = values.get("tt").map(_.asInstanceOf[RawTokens]).getOrElse(RawTokens.empty),
        tags = values.get("tags").map(_.asInstanceOf[List[Tag]]).getOrElse(Nil),
        addInfo = addInfo
      ) :: Nil
    }
  }
}


case class FeedParserDefinition(parserType: ParserType.Value,
                                currency: Currency.Value,
                                feedParsingProperties: Map[String, String],
                                fields: Seq[FieldParserDefinition])

object FieldParserDefinition {
  type RuleParams = Either[String, List[String]]
  type FieldRules = Map[String, RuleParams]
}
case class FieldParserDefinition(feedField: String, mongoField: String, rules: FieldRules)

