package com.alexk.parser

import ru.daron.deal_parser_concept.RawTokens

abstract class FieldParser[R](protected val field: FieldParserDefinition) {
  protected val multiValue: Boolean = false

  def availableRules: Set[ParserRuleFactory[_]]
  private val rulesDiff = field.rules.keySet -- availableRules.map(_.paramName)

  if (rulesDiff.nonEmpty) {
    throw new Exception(s"""Option "${rulesDiff.head} is not allowed for field "${field.mongoField}"""")
  }

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
  val valueListDelimiterRule = callFactory(ValueListDelimiterRuleFactory)

  def parseValue(value: List[String]): Option[R]
}


class StringFieldParser(field: FieldParserDefinition) extends FieldParser[String](field) {
  override def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory)

  override def parseValue(value: List[String]): Option[String] = {
    for {
      v <- readSingleValueRule(value)
    } yield formatRule.map(_.handle(v)).getOrElse(v)
  }
}

class BooleanFieldParser(field: FieldParserDefinition) extends FieldParser[Boolean](field) {
  override def availableRules: Set[ParserRuleFactory[_]] = Set(TrueFormatRuleFactory, FalseFormatRuleFactory)

  override def parseValue(value: List[String]): Option[Boolean] = {
    for {
      v <- readSingleValueRule(value)
      rule <- List(trueFormatRule, falseFormatRule).flatten.headOption
    } yield rule.handle(v)
  }
}

class ArrayParser[T](field: FieldParserDefinition)(implicit view: String => T) extends FieldParser[Seq[T]](field) {
  override def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory, DelimiterRuleFactory)

  override def parseValue(value: List[String]): Option[Seq[T]] = {
    readSingleValueRule(value).map { v =>
      delimiterRule.map(_.handle(v)).getOrElse(Seq(v))
        .map(s => formatRule.map(_.handle(s)).getOrElse(s))
        .map(view)
    }
  }
}

class ValueListParser(field: FieldParserDefinition) extends FieldParser[Seq[String]](field) {
  override def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory, ValueListDelimiterRuleFactory)

  override def parseValue(value: List[String]): Option[Seq[String]] = {
    readSingleValueRule(value).map { v =>
      valueListDelimiterRule.map(_.handle(v)).getOrElse(Seq(v))
        .map(s => formatRule.map(_.handle(s)).getOrElse(s))
    }
  }
}

class TokensParser(field: FieldParserDefinition) extends FieldParser[RawTokens](field) {
  override def availableRules: Set[ParserRuleFactory[_]] = Set(FormatRuleFactory, DelimiterRuleFactory)

  override def parseValue(value: List[String]): Option[RawTokens] = {
    readSingleValueRule(value).map { v =>
      val tokens = delimiterRule.map(_.handle(v)).getOrElse(Seq(v))
        .map(s => formatRule.map(_.handle(s)).getOrElse(s))
      RawTokens(Map.empty, Map("bypass" -> tokens.toList))
    }
  }
}

object AddInfoRules {
  val rules = Set(DelimiterRuleFactory) // TODO key_value_delimiter etc.
}

class AddInfoFieldParser[T](field: FieldParserDefinition, parser: FieldParser[T]) extends FieldParser[Map[String, T]](field) {
  override def availableRules: Set[ParserRuleFactory[_]] = AddInfoRules.rules ++ parser.availableRules

  override def parseValue(value: List[String]): Option[Map[String, T]] = {
    readSingleValueRule(value).map { v => parser.parseValue(v :: Nil).map(field.feedField -> _).toMap
    }
  }
}

class AddInfoBooleanFieldParser(field: FieldParserDefinition) extends AddInfoFieldParser[Boolean](
  field, new BooleanFieldParser(field.copy(
    rules = field.rules.filterNot(r => AddInfoRules.rules.map(_.paramName).contains(r._1))))
)

class AddInfoListFieldParser(field: FieldParserDefinition) extends AddInfoFieldParser[Seq[String]](
  field, new ValueListParser(field.copy(
    rules = field.rules.filterNot(r => AddInfoRules.rules.map(_.paramName).contains(r._1))))
)