package ru.daron.deal_parser_concept

import org.scalatest.FunSuite

class JSFormatterSpec extends FunSuite {

  test("JS formatter") {
    val jsFormatter = JSFormatter[String]("if (p1 == 'world') return 'Hello ' + p1 + ' !!!'; else return p1.toUpperCase();")
    assert(jsFormatter.handle("world") === "Hello world !!!")
    assert(jsFormatter.handle("test") === "TEST")
  }

  test("JS  string formatter") {
    val jsFormatter = JSFormatter[String]("if (p1 == 'world') return 'Hello ' + p1 + ' !!!'; else return p1.toUpperCase();")
    assert(jsFormatter.handle("world") === "Hello world !!!")
    assert(jsFormatter.handle("test") === "TEST")
  }

  test("JS number function") {
    val js = "return p1 * 100;"
    val numberParserRule =  JsNumberFunctionRuleFactory.create(js)
    assert(numberParserRule.handle("5") === 500)
  }

  test("JS boolean function") {
    val js = "if (p1 == 'active') return true; else return false;"
    val booleanParserRule =  JsBooleanFunctionRuleFactory.create(js)
    assert(booleanParserRule.handle("active") === true)
    assert(booleanParserRule.handle("not active") === false)
  }

  test("JS text function") {
    val js = "if (p1.trim() == '') return '-1'; else return p1.trim();"
    val textParserRule =  JsStringFunctionRuleFactory.create(js)
    assert(textParserRule.handle("active") === "active")
    assert(textParserRule.handle(" ") === "-1")
  }
}
