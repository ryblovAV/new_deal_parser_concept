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

    fieldExtractor.readField(fragment, "id") shouldBe "123" :: Nil
    fieldExtractor.readField(fragment, "name") shouldBe "The Product" :: Nil
    fieldExtractor.readField(fragment, "tags.tag") shouldBe "level 1" :: "level 2" :: Nil
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

    fieldExtractor.readField(fragment, "id") shouldBe "123" :: Nil
    fieldExtractor.readField(fragment, "name") shouldBe "The Product" :: Nil
    fieldExtractor.readField(fragment, "tag L1") shouldBe "level 1" :: Nil
    fieldExtractor.readField(fragment, "tag L2") shouldBe "level 2" :: Nil
    fieldExtractor.readField(fragment, "description") shouldBe "" :: Nil
    fieldExtractor.readField(fragment, "available") shouldBe "" :: Nil
  }

  private val csvJson =
    """{
      |  "feed_format": "CSV",
      |  "currency": "USD",
      |  "div": "us",
      |  "lang": "en",
      |  "option_id": {
      |    "db_field": "os._id",
      |    "format": "(size)-(color)"
      |  },
      |  "data": {
      |    "id" : [
      |      {
      |        "column_index": 0,
      |        "db_field" : "_id"
      |      }
      |    ],
      |    "title" : [
      |      {
      |        "column_index": 1,
      |        "db_field" : "t",
      |        "format": "APPEND:;|UPPERCASE"
      |      },
      |      {
      |        "column_index": 1,
      |        "db_field" : "tt",
      |        "delimiter" : [" ", ":"]
      |      }
      |    ],
      |    "description" : [
      |      {
      |        "column_index": 2,
      |        "db_field" : "at"
      |      },
      |      {
      |        "column_index": 2,
      |        "db_field" : "tt",
      |        "delimiter" : [" "],
      |        "format": "PREFIX:abc"
      |      }
      |    ],
      |    "product_subcategory2": [
      |      {
      |        "column_index": 3,
      |        "db_field": "info.cf",
      |        "delimiter": ",",
      |        "key_value_delimiter": [":"],
      |        "value_list_delimiter": ["|"]
      |      }
      |    ],
      |    "new" : [
      |      {
      |        "column_index": 4,
      |        "db_field" : "info.bf",
      |        "true_format" : "yes"
      |      }
      |    ]
      |  }
      |}
    """.stripMargin

  //"format" : "PREFIX:[L1]:[L2]:",
  //"exclude" : ["Ignore", "these", "words"]

  //"exclude" : ["Ignore", "these", "words"],
  //"skip_stop_words" : ["es", "us"]

  val csvLine= "id12345\tThe product title\tLong description : not so long\tWoman: Shoes|Skirts, Men: Hats\tyes"

  test("parse JSON config") {
    val conf = JsonConfigReader.read(csvJson)

    conf.currency shouldBe Currency.USD
    conf.fields should have size 7

    conf.fields.find(_.mongoField == "t").get.rules("format") shouldBe Left("APPEND:;|UPPERCASE")
  }

  test("create parser") {
    val conf = JsonConfigReader.read(csvJson)

    val dealParser = FeedParser(conf).dealParser.asInstanceOf[UniversalDealParser[List[String]]]
    dealParser.fields should have size 7

    val parsed = dealParser.parseDeal(csvLine.split('\t').toList)

    println(parsed)
  }
}
