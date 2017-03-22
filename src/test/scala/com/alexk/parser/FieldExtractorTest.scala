package com.alexk.parser

import org.scalatest.{FunSuite, Matchers}

class FieldExtractorTest extends FunSuite with Matchers {

  test("test xml field extractor") {
    val fieldExtractor = new XmlFieldExtractor()
    val fragment =
      <record>
        <id>123</id>
        <name>The Product</name>
        <tags>
          <tag>level 1</tag>
          <tag>level 2</tag>
        </tags>
      </record>

    fieldExtractor.readField(fragment, "id") should be ("123" :: Nil)
    fieldExtractor.readField(fragment, "name") should be ("The Product" :: Nil)
    fieldExtractor.readField(fragment, "tags.tag") should be ("level 1" :: "level 2" :: Nil)
  }

  test("test csv field extractor") {
    val fields = List(
      CsvFieldDefinition("id", 0),
      CsvFieldDefinition("name", 1),
      CsvFieldDefinition("tag L1", 5),
      CsvFieldDefinition("tag L2", 6),
      CsvFieldDefinition("description", 2),
      CsvFieldDefinition("available", 3)
    )
    val fieldExtractor = CsvFieldExtractor.fromIndices(fields)

    val fragment = List(
      "123",
      "   The Product   ",
      "",
      null,
      "useless field",
      "level 1",
      "level 2"
    )

    fieldExtractor.readField(fragment, "id") should be ("123" :: Nil)
    fieldExtractor.readField(fragment, "name") should be ("The Product" :: Nil)
    fieldExtractor.readField(fragment, "tag L1") should be ("level 1" :: Nil)
    fieldExtractor.readField(fragment, "tag L2") should be ("level 2" :: Nil)
    fieldExtractor.readField(fragment, "description") should be ("" :: Nil)
    fieldExtractor.readField(fragment, "available") should be ("" :: Nil)
  }

  test("parse JSON config") {
    val json =
      """{
        |  "feed_format": "CSV",
        |  "currency": "USD",
        |  "div": "us",
        |  "lang": "en",
        |  "option_id": {
        |    "db_field": "os.dis._id",
        |    "format": "(size)-(color)"
        |  },
        |  "data": {
        |    "id" : [
        |      {
        |        "column_index":0,
        |        "db_field" : "_id"
        |      }
        |    ],
        |    "title" : [
        |      {
        |        "db_field" : "t",
        |        "option":true
        |      },
        |      {
        |        "db_field" : "tt",
        |        "delimiter" : [" ", ":"],
        |        "format" : "PREFIX:[L1]:[L2]:",
        |        "exclude" : ["Ignore", "these", "words"]
        |      }
        |    ],
        |    "description" : [
        |      {
        |        "db_field" : "at"
        |      },
        |      {
        |        "db_field" : "tt",
        |        "delimiter" : [" "],
        |        "exclude" : ["Ignore", "these", "words"],
        |        "skip_stop_words" : ["es", "us"]
        |      }
        |    ]
        |  }
        |}
      """.stripMargin

    val conf = JsonConfigReader.createParser(json)
  }
}
