package ru.daron.deal_parser_concept

import javax.script.{Compilable, Invocable, ScriptEngineManager}

import com.alexk.parser.{ParserRule, ParserRuleFactory}

object FormatRuleFactory extends ParserRuleFactory[String] {

  val PREFIX = "PREFIX"
  val APPEND = "APPEND"
  val REMOVE = "REMOVE"
  val REPLACE = "REPLACE"
  val UPPERCASE = "UPPERCASE"
  val LOWERCASE = "LOWERCASE"
  val JS_FUNC = "JS_FUNC"

  override val paramName: String = "format"

  override val paramsIsArray: Boolean = false

  def createOne(param: String): ParserRule[String] = param.trim.split(':').toList match {
    case List(PREFIX, prefix) => PrefixFormatter(prefix)
    case List(APPEND, append) => AppendFormatter(append)
    case List(REMOVE, remove) => RemoveFormatter(remove)
    case List(REPLACE, str) =>
      str.split('~') match {
        case Array(source, replacement) => ReplaceFormatter(source, replacement)
        case Array(source) => ReplaceFormatter(source, "")
        case _ => throw new Exception(s"Wrong config for replacement formatter: $str")
      }
    case List(UPPERCASE) => UpperCaseFormatter
    case List(LOWERCASE) => LowerCaseFormatter
    case JS_FUNC::jsCode => JSFormatter[String](jsCode.mkString(":"))
    case _ => throw new Exception(s"unknown formatter: $param")
  }

  override def create(param: String): ParserRule[String] = {
    val formatters = param.split('|').map(FormatRuleFactory.createOne).toList
    CompositeFormatter(formatters)
  }
}

case class CompositeFormatter(formatters: List[ParserRule[String]]) extends ParserRule[String] {
  override def handle(value: String): String = {
    formatters.foldLeft(value)((acc, formatter) => formatter.handle(acc))
  }
}

case class PrefixFormatter(prefix: String) extends ParserRule[String]  {
  override def handle(value: String): String = prefix + value
}

case class AppendFormatter(append: String) extends ParserRule[String] {
  override def handle(value: String): String = value + append
}

case class RemoveFormatter(remove: String) extends ParserRule[String] {
  override def handle(value: String): String = value.replaceAll(remove, "")
}

case class ReplaceFormatter(source: String, replacement: String) extends ParserRule[String] {
  override def handle(value: String): String = value.replaceAll(source, replacement)
}

case object UpperCaseFormatter extends ParserRule[String] {
  override def handle(value: String): String = value.toUpperCase
}

case object LowerCaseFormatter extends ParserRule[String] {
  override def handle(value: String): String = value.toLowerCase
}