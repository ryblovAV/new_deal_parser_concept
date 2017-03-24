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

//trait FieldParserBase[R] {
//  protected def availableRules: Seq[String] = Seq.empty
//  def parseValue(value: List[String]): Seq[(String, R)]
//}

abstract class FieldParser[R](protected val field: FieldParserDefinition) {
  protected val multiValue: Boolean = false

  protected def availableRules: Set[ParserRuleFactory[_]]
  private val rulesDiff = field.rules.keySet -- availableRules.map(_.paramName)

  if (rulesDiff.nonEmpty) {
    throw new Exception(s"""Option "${rulesDiff.head} is not allowed for field "${field.mongoField}"""")
  }

//  val rules: Seq[ParserRule[_]] = availableRules.flatMap { s => s match {
//    case "format" => fieldRules.get(s).map(new FormatRule(_))
//    case "true_format" => fieldRules.get(s).map(new BooleanTrueRule(_))
//    case "false_format" => fieldRules.get(s).map(new BooleanFalseRule(_))
//    case "delimiter" => fieldRules.get(s).map(new DelimiterRule(_))
//  }}

  def callFactory[T](f: ParserRuleFactory[T]): Option[ParserRule[T]] = {
    try {
      f(field.rules)
    } catch {
      case e: Exception => throw new Exception(s"""Error in "${field.feedField}" field definition: ${e.getMessage}""")
    }
  }

  val readSingleValueRule: List[String] => Option[String] = value => if (value.size > 1)
    throw new Exception(s"""Only one value allowed for field "1"""")
  else
    value.headOption.map(_.trim).filter(_.nonEmpty)

  val formatRule = callFactory(FormatRuleFactory)
  val trueFormatRule = callFactory(TrueFormatRuleFactory)
  val falseFormatRule = callFactory(FalseFormatRuleFactory)
  val delimiterRule = callFactory(DelimiterRuleFactory)

//  def parse(value: List[String]): Option[R]

  def parseValue(value: List[String]): Option[R] //= {

    // else ...
//    val initFieldValue = FieldValue(field.mongoField)

    //initValue.map(s => rules.foldLeft(s) { case (v, rule) => rule.handle(v)})

//    initValue.toList.map(v => field.mongoField -> //v.asInstanceOf[R])
//    rules.headOption.map(rule => rule.asInstanceOf[ParserRule[R]].handle(v)).getOrElse(v.asInstanceOf[R]))

//    initValue.toList.map { v =>
//      rules.foldLeft(v) { case (acc, rule) => rule.})
//  }

  //  def acceptedType: Class[_]
}

//trait InfoFieldParser[R] extends FieldParser[R] {
//  override def parseValue(value: List[String]): Seq[(String, R)] = {
//    super.parseValue(value).map { case (mf, v) => mf -> Map() }
//  }
//}
//class SingleValueParser(field: FieldParserDefinition) extends FieldParser[Option[String]](field) {
//  override def parseValue(value: List[String]): Option[String] = {
//      if (value.size > 1)
//        throw new Exception(s"""Only one value allowed for field "1"""")
//      else
//        value.headOption.map(_.trim).filter(_.nonEmpty)
//  }
//}

class StringFieldParser(field: FieldParserDefinition) extends FieldParser[String](field) {
  override protected def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory)

  override def parseValue(value: List[String]): Option[String] = {
    for {
      v <- readSingleValueRule(value)
    } yield formatRule.map(_.handle(v)).getOrElse(v)
  }
}

class BooleanFieldParser(field: FieldParserDefinition) extends FieldParser[Boolean](field) {
  override protected def availableRules: Set[ParserRuleFactory[_]] = Set(TrueFormatRuleFactory, FalseFormatRuleFactory)

  override def parseValue(value: List[String]): Option[Boolean] = {
    for {
      v <- readSingleValueRule(value)
      rule <- List(trueFormatRule, falseFormatRule).flatten.headOption
    } yield rule.handle(v)
  }
}

class ArrayParser[T](field: FieldParserDefinition)(implicit view: String => T) extends FieldParser[Seq[T]](field) {
  override protected def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory, DelimiterRuleFactory)

  override def parseValue(value: List[String]): Option[Seq[T]] = {
    readSingleValueRule(value).map { v =>
      delimiterRule.map(_.handle(v)).getOrElse(Seq(v))
        .map(s => formatRule.map(_.handle(s)).getOrElse(s))
        .map(view)
    }
  }
}

class TokensParser(field: FieldParserDefinition) extends FieldParser[RawTokens](field) {
  override protected def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory, DelimiterRuleFactory)

  override def parseValue(value: List[String]): Option[RawTokens] = {
    readSingleValueRule(value).map { v =>
      val tokens = delimiterRule.map(_.handle(v)).getOrElse(Seq(v))
        .map(s => formatRule.map(_.handle(s)).getOrElse(s))
      RawTokens(Map.empty, Map("bypass" -> tokens.toList))
    }
  }
}

//trait ValueHandler[R] {
//  def handle(value: String): R
//}
//class

//class FieldValue[T](val mongoField: String, val value: T)

trait ParserRule[T] {
  def handle(value: String): T
}

trait ParserRuleFactory[T] {
  val paramName: String
  val paramsIsArray: Boolean = false

  def apply(f: FieldRules): Option[ParserRule[T]] = {
    f.get(paramName).map { params =>
      if (paramsIsArray)
        create(params.right.get)
      else
        create(params.left.get)
    }
  }

  def create(param: String): ParserRule[T] = ???
  def create(params: Seq[String]): ParserRule[T] = ???
}

object FormatRuleFactory extends ParserRuleFactory[String] {
  override val paramName: String = "format"

  override def create(param: String): ParserRule[String] = {
    // will throw exception in case of corrupted config
    Formatter.process("test", param)

    (value: String) => Formatter.process(value, param)
  }
}

object TrueFormatRuleFactory extends ParserRuleFactory[Boolean] {
  override val paramName: String = "true_format"

  override def create(param: String): ParserRule[Boolean] = {
    (value: String) => value.toLowerCase == param.toLowerCase
  }

  override def apply(f: FieldRules): Option[ParserRule[Boolean]] = {
    super.apply(f).map(
      if (f.contains(FalseFormatRuleFactory.paramName))
        throw new Exception("true_format and false_format can't be specified together")
      else _
    )
  }
}

object FalseFormatRuleFactory extends ParserRuleFactory[Boolean] {
  override val paramName: String = "false_format"

  override def create(param: String): ParserRule[Boolean] = {
    (value: String) => value.toLowerCase != param.toLowerCase
  }

  override def apply(f: FieldRules): Option[ParserRule[Boolean]] = {
    super.apply(f).map(
      if (f.contains(TrueFormatRuleFactory.paramName))
        throw new Exception("true_format and false_format can't be specified together")
      else _
    )
  }
}

object DelimiterRuleFactory extends ParserRuleFactory[Seq[String]] {
  override val paramName: String = "delimiter"
  override val paramsIsArray = true

  override def create(delimiters: Seq[String]): ParserRule[Seq[String]] = {
    (value: String) =>
      delimiters.foldLeft(Array(value)) { case (arr, delimiter) =>
        arr.flatMap(_.split(delimiter))
      }.toList
  }
}


object FieldParserFactory {
  def apply(field: FieldParserDefinition): FieldParser[_] = {
    field.mongoField match {
      case "_id" => new StringFieldParser(field)
      case "t" => new StringFieldParser(field)
      case "at" => new StringFieldParser(field)
      case "tt" => new TokensParser(field)
      case "tags" => new ArrayParser[Tag](field)(Tag)
      case "info.cf" => null
      case "info.bf" => new BooleanFieldParser(field)
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
//        booleanFields = values.get("info.bf").map(_.asInstanceOf[Map[String,Boolean]]).getOrElse(Map.empty)
        //listFields = values.get("info.cf").map(_.asInstanceOf[Map[String,List[String]]]).getOrElse(Map.empty)
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

