package com.alexk.parser

import java.io.{InputStream, InputStreamReader}
import java.nio.charset.Charset

import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import ru.daron.deal_parser_concept.RawDeal

import scala.xml.Node

object FeedParser {
  def apply(feedConfig: FeedParserDefinition): FeedParser = {

    val fieldParsers = feedConfig.fields.map(definition =>
      (definition.feedField, definition.mongoField, FieldParserFactory(definition.copy(rules = definition.rules - "column_index")))
    )

    feedConfig.parserType match {
      case ParserType.XML =>
        val xmlDealParser = new UniversalDealParser[Node](feedConfig.currency, new XmlFieldExtractor(), Seq.empty)

        val feedParser = ???
        null

      case ParserType.CSV =>

        val fields = feedConfig.fields.map { f =>
          val index = f.rules.getOrElse("column_index",
            throw new Exception("column_index should be specified for every column for CSV feeds"))
            .left.getOrElse(throw new Exception("column_index shoud have only one value"))
            .toInt
          CsvFieldDefinition(f.feedField, index)
        }

        new FeedParser {
          override val dealParser = new UniversalDealParser[List[String]](
            feedConfig.currency,
            CsvFieldExtractor.fromIndices(fields),
            fieldParsers)

          override def parse(stream: InputStream): Iterator[RawDeal] = {
            val reader = new InputStreamReader(stream, Charset.forName("utf-8"))
            val (quoteChar, delimiterChar, endOfLineSymbol) = ('"', '\t', "\n")
            val csvReader = new CsvListReader(reader, new CsvPreference.Builder(quoteChar, delimiterChar, endOfLineSymbol).build)
            import scala.collection.convert.wrapAsScala._

            val iterator: Iterator[List[String]] = new Iterator[java.util.List[String]] {
              override def hasNext: Boolean = true

              override def next(): java.util.List[String] = csvReader.read()
            }.takeWhile(_ != null).map(_.toList)

            iterator.flatMap(line => dealParser.parseDeal(line).toOption.toList.flatten)
          }
        }
    }
  }
}

trait FeedParser {
  val dealParser: UniversalDealParser[_]
  def parse(stream: InputStream): Iterator[RawDeal]

  // TODO close
}