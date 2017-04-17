package ru.daron.deal_parser_concept

import javax.script.{Compilable, Invocable, ScriptEngineManager}

import com.alexk.parser.{ParserRule, ParserRuleFactory}

object JSFormatterEngine {
  def build(js: String): Invocable = {
    val engine = new ScriptEngineManager(null).getEngineByMimeType("text/javascript")
    val compilable = engine.asInstanceOf[Compilable]
    val invocable = engine.asInstanceOf[Invocable]
    val statement = s"function test(p1) {$js};"
    val compiled = compilable.compile(statement)

    compiled.eval()
    invocable
  }
}

case class JSFormatter[T](js: String) extends ParserRule[T] {
  import JSFormatterEngine._
  private val invocable = build(js)

  override def handle(value: String): T = invocable.invokeFunction("test", value).asInstanceOf[T]
}

object JsNumberFunctionRuleFactory extends ParserRuleFactory[Double] {
  override val paramName: String = "number_function"
  override val paramsIsArray: Boolean = false

  override def create(param: String): ParserRule[Double] = JSFormatter[Double](param)
}

object JsStringFunctionRuleFactory extends ParserRuleFactory[String] {
  override val paramName: String = "text_function"
  override val paramsIsArray: Boolean = false

  override def create(param: String): ParserRule[String] = JSFormatter[String](param)
}


object JsBooleanFunctionRuleFactory extends ParserRuleFactory[Boolean] {
  override val paramName: String = "boolean_function"
  override val paramsIsArray: Boolean = false

  override def create(param: String): ParserRule[Boolean] =  JSFormatter[Boolean](param)
}