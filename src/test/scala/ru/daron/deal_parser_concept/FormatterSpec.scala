package ru.daron.deal_parser_concept

import org.scalatest.FunSuite

class FormatterSpec extends FunSuite {

  test("PrefixFormatter"){
    val formatter = PrefixFormatter("abc_")
    val in = "blahThisIsMyName"
    val out = "abc_blahThisIsMyName"
    assert(formatter.handle(in) === out)
  }

  test("AppendFormatter"){
    val formatter = AppendFormatter("_abc")
    val in = "blahThisIsMyName"
    val out = "blahThisIsMyName_abc"
    assert(formatter.handle(in) === out)
  }

  test("RemoveFormatter"){
    val formatter = RemoveFormatter("blah")
    val in = "blahThisIsMyName"
    val out = "ThisIsMyName"
    assert(formatter.handle(in) === out)
  }

  test("ReplaceFormatter"){
    val formatter = ReplaceFormatter("blah","")
    val in = "blahThisIsMyName"
    val out = "ThisIsMyName"
    assert(formatter.handle(in) === out)
  }

  test("ReplaceFormatter2"){
    val formatter = ReplaceFormatter("MyName","YourName")
    val in = "blahThisIsMyName"
    val out = "blahThisIsYourName"
    assert(formatter.handle(in) === out)
  }

  test("UpperCaseFormatter"){
    val formatter = UpperCaseFormatter
    val in = "blahThisIsMyName"
    val out = "BLAHTHISISMYNAME"
    assert(formatter.handle(in) === out)
  }

  test("LowerCaseFormatter"){
    val formatter = LowerCaseFormatter
    val in = "blahThisIsMyName"
    val out = "blahthisismyname"
    assert(formatter.handle(in) === out)
  }

  test("build parser") {
    assert(FormatRuleFactory.createOne("PREFIX:abc") === PrefixFormatter("abc"))
    assert(FormatRuleFactory.createOne("APPEND:xyz") === AppendFormatter("xyz"))
    assert(FormatRuleFactory.createOne("REMOVE:z-blah") === RemoveFormatter("z-blah"))
    assert(FormatRuleFactory.createOne("REPLACE:MyName~YourName") === ReplaceFormatter("MyName","YourName"))
    assert(FormatRuleFactory.createOne("REPLACE:MyName~") === ReplaceFormatter("MyName",""))
    assert(FormatRuleFactory.createOne("UPPERCASE") === UpperCaseFormatter)
    assert(FormatRuleFactory.createOne("LOWERCASE") === LowerCaseFormatter)
    val js = "var a = p1.split(\":\"); if (a.length > 1) return a[1]; else return \"\";"
    assert(FormatRuleFactory.createOne(s"JS_FUNC:$js") === JSFormatter(js))
  }

  test("process parser") {
    val strConfig = "PREFIX:abcz- | APPEND:xyz | REMOVE:z-blah | REPLACE:MyName~YourName | UPPERCASE"
    val in = "blahThisIsMyName"
    val out = "ABCTHISISYOURNAMEXYZ"
    val formatter = FormatRuleFactory.create(strConfig)
    assert(formatter.handle(in) === out)
  }

}
