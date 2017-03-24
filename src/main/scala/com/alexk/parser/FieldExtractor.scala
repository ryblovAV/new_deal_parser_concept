package com.alexk.parser

import com.alexk.parser.FieldParserDefinition.{FieldRules, RuleParams}
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

trait FieldParserBase[R] {
  protected def availableRules: Seq[String] = Seq.empty
  def parseValue(value: List[String]): Seq[(String, R)]
}

abstract class FieldParser[R](protected val field: FieldParserDefinition) extends FieldParserBase[R] {
  protected val multiValue: Boolean = false

  private val fieldRules = field.rules
  private val rulesDiff = fieldRules.keySet -- availableRules.toSet

  if (rulesDiff.nonEmpty) {
    throw new Exception(s"""Option "${rulesDiff.head} is not allowed for field "${field.mongoField}"""")
  }

  val rules: Seq[ParserRule[_]] = availableRules.flatMap { s => s match {
    case "format" => fieldRules.get(s).map(new FormatRule(_))
    case "true_format" => fieldRules.get(s).map(new BooleanTrueRule(_))
    case "false_format" => fieldRules.get(s).map(new BooleanFalseRule(_))
    case "delimiter" => fieldRules.get(s).map(new DelimiterRule(_))
  }}

  //  def parse(value: List[String]): Option[R]

  def parseValue(value: List[String]): Seq[(String, R)] = {
    val initValue =
    //    if (!multiValue)
      if (value.size > 1)
        throw new Exception(s"""Only one value allowed for field "1"""")
      else
        value.headOption.map(_.trim).filter(_.nonEmpty)
    // else ...
//    val initFieldValue = FieldValue(field.mongoField)

    //initValue.map(s => rules.foldLeft(s) { case (v, rule) => rule.handle(v)})
    initValue.toList.map(v => field.mongoField -> //v.asInstanceOf[R])
    rules.headOption.map(rule => rule.asInstanceOf[ParserRule[R]].handle(v)).getOrElse(v.asInstanceOf[R]))
//    initValue.toList.map { v =>
//      rules.foldLeft(v) { case (acc, rule) => rule.})
  }

  //  def acceptedType: Class[_]
}

//trait InfoFieldParser[R] extends FieldParser[R] {
//  override def parseValue(value: List[String]): Seq[(String, R)] = {
//    super.parseValue(value).map { case (mf, v) => mf -> Map() }
//  }
//}

class StringFieldParser(field: FieldParserDefinition) extends FieldParser[String](field) {


  //  override def parse(value: List[String]): Option[String] = {
  //    val raw =
  //
  //    raw
  //  }
  override protected def availableRules: Seq[String] = Seq("format")
}

class BooleanFieldParser(field: FieldParserDefinition) extends FieldParser[Boolean](field) {
  override protected def availableRules: Seq[String] = Seq("true_format", "false_format")
  //  override def parse(value: List[String]): Option[Boolean] = {
  //    None
  //  }
}

class ArrayParser(field: FieldParserDefinition) extends FieldParser[Seq[String]](field) {
  override protected def availableRules: Seq[String] = Seq("delimiter")
}

//trait ValueHandler[R] {
//  def handle(value: String): R
//}
//class

//class FieldValue[T](val mongoField: String, val value: T)

abstract class ParserRule[R] {
  def handle(value: String): R
}
//  val availableRules = Set.empty[String]

//class ReadSingleValueRule extends ParserRule[List[String], Option[String]] {
//  override def handle(value: List[String]): Option[String] = if (value.size > 1)
//    throw new Exception(s"""Only one value allowed for field "1"""")
//  else
//    value.headOption.map(_.trim).filter(_.nonEmpty)
//}

//  def apply(fieldParserDefinition: FieldParserDefinition): ParserRule[T, R] = {
//    val rulesDiff = fieldParserDefinition.rules.keySet -- availableRules
//
//    if (rulesDiff.nonEmpty) {
//      throw new Exception(s"""Option "${rulesDiff.head} is not allowed for field "${fieldParserDefinition.mongoField}"""")
//    }
//
//    this
//  }


class FormatRule(params: RuleParams) extends ParserRule[String] {
  private val config = params.left.get
  Formatter.process("test", config) // will throw exception in case of corrupted config
  override def handle(value: String): String = Formatter.process(value, config)
}

class BooleanTrueRule(params: RuleParams) extends ParserRule[Boolean] {
  protected val config: String = params.left.get
  override def handle(value: String): Boolean = value.toLowerCase == config.toLowerCase
}

class BooleanFalseRule(params: RuleParams) extends BooleanTrueRule(params) {
  override def handle(value: String): Boolean = !super.handle(value)
}

class DelimiterRule(params: RuleParams) extends ParserRule[Seq[String]] {
  private val delimiters = params.right.get
  override def handle(value: String): Seq[String] = delimiters.foldLeft(Array(value)) { case (arr, delimiter) =>
    arr.flatMap(_.split(delimiter)) }.toList
}

// delimiter
// true format
// bool
// tt
// cf


object FieldParserFactory {
  def apply(field: FieldParserDefinition): FieldParserBase[_] = {
    field.mongoField match {
      case "_id" => new StringFieldParser(field)
      case "t" => new StringFieldParser(field)
      case "at" => new StringFieldParser(field)
      case "tt" => null //new ArrayParser(field)
      case "tags" => new ArrayParser(field)
      case "info.cf" => null
      case "info.bf" => new BooleanFieldParser(field)
      case "ia" => new BooleanFieldParser(field)
    }
  }
}

class UniversalDealParser[T](currency: Currency.Value,
                             fieldExtractor: FieldExtractor[T],
                             val fields: Seq[(String, String, FieldParserBase[_])]) {

  def parseDeal(in: T): Try[List[RawDeal]] = {
    Try {
      val valuesList: Seq[(String, _)] = fields
        .toList.flatMap { case (field, mongoField, rule) =>
        val value = fieldExtractor.readField(in, field)
        if (rule != null) rule.parseValue(value) else Nil // TODO remove NULL check
      }

      // merge by key
      val values: Map[String, Any] = valuesList.groupBy(_._1).map { case (key, list) =>
          val definedValues = list.map(_._2)
          if (definedValues.size < 2)
            list.head
          else {
            if (definedValues.head.isInstanceOf[Seq[_]])
              key -> Some(definedValues.flatMap(_.asInstanceOf[Seq[_]]))
            else
              throw new Exception(s"Multiple values found for a field that allows a single value only: $key")
          }
      }

      val addInfo = AddInfo.empty.copy(
//        booleanFields = values.get("info.bf").map(_.asInstanceOf[Map[String,Boolean]]).getOrElse(Map.empty)
        //listFields = values.get("info.cf").map(_.asInstanceOf[Map[String,List[String]]]).getOrElse(Map.empty)
      )

      RawDeal.empty.copy(
        id = values("_id").asInstanceOf[String],
        title = values.get("t").map(_.asInstanceOf[String]),
        description = values.get("at").map(_.asInstanceOf[String]),
        isActive = values.get("is").map(_.asInstanceOf[Boolean]).getOrElse(false), // should be true really
        rawTokens = values.get("tt").map(_.asInstanceOf[RawTokens]).getOrElse(RawTokens.empty),
        tags = values.get("tags").map(_.asInstanceOf[List[String]].map(Tag)).getOrElse(Nil),
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

