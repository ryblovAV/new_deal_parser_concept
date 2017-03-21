package ru.daron.deal_parser_concept

import org.scalatest.FunSuite

class FormatterSpec extends FunSuite {

  test("PrefixFormatter"){
    val formatter = PrefixFormatter("abc_")
    val in = "blahThisIsMyName"
    val out = "abc_blahThisIsMyName"
    assert(formatter(in) === out)
  }

  test("AppendFormatter"){
    val formatter = AppendFormatter("_abc")
    val in = "blahThisIsMyName"
    val out = "blahThisIsMyName_abc"
    assert(formatter(in) === out)
  }

  test("RemoveFormatter"){
    val formatter = RemoveFormatter("blah")
    val in = "blahThisIsMyName"
    val out = "ThisIsMyName"
    assert(formatter(in) === out)
  }

  test("ReplaceFormatter"){
    val formatter = ReplaceFormatter("blah","")
    val in = "blahThisIsMyName"
    val out = "ThisIsMyName"
    assert(formatter(in) === out)
  }

  test("ReplaceFormatter2"){
    val formatter = ReplaceFormatter("MyName","YourName")
    val in = "blahThisIsMyName"
    val out = "blahThisIsYourName"
    assert(formatter(in) === out)
  }

  test("UpperCaseFormatter"){
    val formatter = UpperCaseFormatter
    val in = "blahThisIsMyName"
    val out = "BLAHTHISISMYNAME"
    assert(formatter(in) === out)
  }

  test("LowerCaseFormatter"){
    val formatter = LowerCaseFormatter
    val in = "blahThisIsMyName"
    val out = "blahthisismyname"
    assert(formatter(in) === out)
  }

  test("build parser") {
    assert(Formatter.create("PREFIX:abc") === PrefixFormatter("abc"))
    assert(Formatter.create("APPEND:xyz") === AppendFormatter("xyz"))
    assert(Formatter.create("REMOVE:z-blah") === RemoveFormatter("z-blah"))
    assert(Formatter.create("REPLACE:MyName~YourName") === ReplaceFormatter("MyName","YourName"))
    assert(Formatter.create("REPLACE:MyName~") === ReplaceFormatter("MyName",""))
    assert(Formatter.create("UPPERCASE") === UpperCaseFormatter)
    assert(Formatter.create("LOWERCASE") === LowerCaseFormatter)
  }

  test("process parser") {
    val strConfig = "PREFIX:abcz- | APPEND:xyz | REMOVE:z-blah | REPLACE:MyName~YourName | UPPERCASE"
    val in = "blahThisIsMyName"
    val out = "ABCTHISISYOURNAMEXYZ"
    assert(Formatter.process(in, strConfig) === out)
  }


}
