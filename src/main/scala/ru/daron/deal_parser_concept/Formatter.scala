package ru.daron.deal_parser_concept

import javax.script.{Compilable, Invocable, ScriptEngineManager}

sealed trait Formatter {
  def apply(in: String): String
}

object Formatter {

  val PREFIX = "PREFIX"
  val APPEND = "APPEND"
  val REMOVE = "REMOVE"
  val REPLACE = "REPLACE"
  val UPPERCASE = "UPPERCASE"
  val LOWERCASE = "LOWERCASE"

  def process(in: String, configStr: String): String = {
    configStr.split('|').map(create).foldLeft(in)((acc, formatter) => formatter(acc))
  }

  def create(configStr: String): Formatter = configStr.trim.split(':') match {
    case Array(PREFIX, prefix) => PrefixFormatter(prefix)
    case Array(APPEND, append) => AppendFormatter(append)
    case Array(REMOVE, remove) => RemoveFormatter(remove)
    case Array(REPLACE, str) =>
      str.split('~') match {
        case Array(source, replacement) => ReplaceFormatter(source, replacement)
        case Array(source) => ReplaceFormatter(source, "")
        case _ => throw new Exception(s"Wrong config for replacement formatter: $str")
      }
    case Array(UPPERCASE) => UpperCaseFormatter
    case Array(LOWERCASE) => LowerCaseFormatter
    case _ => throw new Exception(s"unknown formatter: $configStr")
  }
}

case class JSFormatter(js: String) extends Formatter {

  private val invocable = {
    val engine = new ScriptEngineManager().getEngineByName("nashorn")
    val compilable = engine.asInstanceOf[Compilable]
    val invocable = engine.asInstanceOf[Invocable]
    val statement = s"function test(p1) {$js};"
    val compiled = compilable.compile(statement)

    compiled.eval()
    invocable
  }

  override def apply(in: String): String = {
    invocable.invokeFunction("test", in).asInstanceOf[String]
  }
}


case class PrefixFormatter(prefix: String) extends Formatter {
  override def apply(in: String): String = prefix + in
}

case class AppendFormatter(append: String) extends Formatter {
  override def apply(in: String): String = in + append
}

case class RemoveFormatter(remove: String) extends Formatter {
  override def apply(in: String): String = in.replaceAll(remove, "")
}

case class ReplaceFormatter(source: String, replacement: String) extends Formatter {
  override def apply(in: String): String = in.replaceAll(source, replacement)
}

case object UpperCaseFormatter extends Formatter {
  override def apply(in: String): String = in.toUpperCase
}

case object LowerCaseFormatter extends Formatter {
  override def apply(in: String): String = in.toLowerCase
}
