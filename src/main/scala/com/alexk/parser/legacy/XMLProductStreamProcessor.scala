package com.alexk.parser.legacy

import java.io.InputStream
import java.nio.charset.CodingErrorAction

import org.xml.sax.helpers.{DefaultHandler, XMLFilterImpl, XMLReaderFactory}
import org.xml.sax.{Attributes, InputSource, XMLReader}

import scala.io.Codec

class XMLProductStreamProcessor(productElementName: String, escapeXmlElemText: Boolean = false) extends StreamProcessor {
  val DOUBLE_QUOTES = "\""

  private def attributesToString(attributes: Attributes): String = {
    if (attributes.getLength > 0) {
      val attributesText = new StringBuilder
      for (idx <- 0 until attributes.getLength ) {
        attributesText.append(" ")
        val attributeValue = StringUtils.escapeXMLEntities(StringUtils.unescapeXMLEntities(attributes.getValue(idx)))
        attributesText.append(s"${attributes.getQName(idx)}=$DOUBLE_QUOTES$attributeValue$DOUBLE_QUOTES")
      }
      attributesText.toString()
    } else {
      ""
    }
  }

  def consume(inputStream: InputStream, encoding: String)(process: String => Unit): Unit = {

    val codec = Codec(encoding)
    codec.onMalformedInput(CodingErrorAction.IGNORE)
    codec.onUnmappableCharacter(CodingErrorAction.IGNORE)

    var stringBuffer: StringBuilder = null
    var processingNewElement: Boolean = false
    val ELEM = productElementName

    val handler = new DefaultHandler() {
      override def characters(ch: Array[Char], start: Int, length: Int) = {
        if (processingNewElement) {
          if (escapeXmlElemText) {
            stringBuffer.append(StringUtils.escapeXMLEntities(StringUtils.unescapeXMLEntities(ch.slice(start, start + length).mkString)))
          } else {
            stringBuffer.append(ch.slice(start, start + length).mkString)
          }
        }
      }

      override def endElement(uri: String, localName: String, qName: String) = {
        if (processingNewElement) {
          stringBuffer.append("</" + qName + ">")
        }
        if (qName.equals(ELEM)) {
          processingNewElement = false
          process(stringBuffer.toString())
          stringBuffer = null
        }
      }

      override def startElement(uri: String, localName: String, qName: String, attributes: Attributes) = {
        if (qName.equals(ELEM)) {
          processingNewElement = true
          stringBuffer = new StringBuilder
        }
        if (processingNewElement) {
          stringBuffer.append("<" + qName + attributesToString(attributes) + ">")
        }
      }
    }

    val xmlFilter = new XMLFilterEntityImpl(XMLReaderFactory.createXMLReader())
    xmlFilter.setContentHandler(handler)
    xmlFilter.parse(new InputSource(new SourceInputStream(scala.io.Source.fromInputStream(inputStream)(codec))))
  }

  import org.xml.sax.ext.LexicalHandler
  private class XMLFilterEntityImpl(reader: XMLReader) extends XMLFilterImpl with LexicalHandler {
    private var currentEntity: String = null
    super.setParent(reader)
    setProperty("http://xml.org/sax/properties/lexical-handler", this)

    override def characters(ch: Array[Char], start: Int, end: Int): Unit = {
      if (currentEntity == null) {
        super.characters(ch, start, end)
      } else {
        val entity = s"&$currentEntity;"
        super.characters(entity.toCharArray, 0, entity.length)
        currentEntity = null
      }
    }

    override def endEntity(name: String): Unit = {}

    override def comment(ch: Array[Char], start: Int, length: Int): Unit = {}

    override def startCDATA(): Unit = {}

    override def endDTD(): Unit = {}

    override def endCDATA(): Unit = {}

    override def startEntity(name: String): Unit = {
      currentEntity = name
    }

    override def startDTD(name: String, publicId: String, systemId: String): Unit = {}
  }

  /**
   * Name of stream processor
   * @return processor name
   */
  override def name: String = "xml processor"
}