package com.alexk.parser

import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.Try

object JsonConfigReader {

  case class JsonConfiguration(feedFormat: String, currency: String, data: JsObject)

  private implicit val configurationReads: Reads[JsonConfiguration] = (
    (JsPath \ "feed_format").read[String] and
      (JsPath \ "currency").read[String] and
      (JsPath \ "data").read[JsObject]
    ) (JsonConfiguration.apply _)

  private def readConfig(jsonString: String): JsonConfiguration = Json.parse(jsonString).validate[JsonConfiguration] match {
    case s: JsSuccess[JsonConfiguration] => s.get
    case e: JsError => throw new Exception(s"Can't read JSON parser configuration: $e")
  }

  private def parseFieldDefinition(fieldName: String, json: JsValue): FieldParserDefinition = {
    val rules = json.as[JsObject].value.map { case (k, v) => k -> (v match {
      case s: JsString => Left(s.value.trim)
      case b: JsBoolean => Left(b.toString)
      case n: JsNumber => Left(n.toString)
      case a: JsArray => Right(a.value.map(_.as[String].trim).toList)
      case _ => throw new Exception(s"""No value specified for field "$fieldName", rule "$k"""")
    })
    }.toMap

    val mongoField = Try(rules("db_field").left.get).getOrElse(
      throw new Exception(s""""db_field" must be specified for field "$fieldName""""))

    FieldParserDefinition(fieldName, mongoField, rules - "db_field")
  }

  def read(jsonString: String): FeedParserDefinition = {
    val config: JsonConfiguration = readConfig(jsonString)

    val parserType = ParserType.withName(config.feedFormat)
    val currency = Currency.withName(config.currency)

    val fieldDefinitions = config.data.fields.flatMap { case (name, jsValue) =>
      jsValue.validate[JsArray] match {
        case s: JsSuccess[JsArray] => s.get.value.map(parseFieldDefinition(name, _))
        case e: JsError => throw new Exception(s"Field definition must be an array: $e")
      }
    }

    FeedParserDefinition(parserType, currency, Map.empty, fieldDefinitions)
  }
}
