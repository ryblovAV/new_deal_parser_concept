package com.alexk.parser

import com.alexk.parser.FieldParserDefinition.FieldRules
import ru.daron.deal_parser_concept.Formatter

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

object DelimiterRuleFactory extends ParserRuleFactory[List[String]] {
  override val paramName: String = "delimiter"
  override val paramsIsArray = true

  override def create(delimiters: Seq[String]): ParserRule[List[String]] = {
    (value: String) =>
      delimiters.foldLeft(Array(value)) { case (arr, delimiter) =>
        arr.flatMap(_.split(delimiter))
      }.toList
  }
}

object ValueListDelimiterRuleFactory extends ParserRuleFactory[List[String]] {
  override val paramName: String = "value_list_delimiter"
  override val paramsIsArray = true

  override def create(delimiters: Seq[String]): ParserRule[List[String]] = {
    (value: String) =>
      delimiters.foldLeft(Array(value)) { case (arr, delimiter) =>
        arr.flatMap(_.split(s"[$delimiter]"))
      }.toList
  }
}
