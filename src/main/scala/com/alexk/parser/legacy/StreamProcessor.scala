package com.alexk.parser.legacy

import java.io.InputStream

/**
 * Stream Processor. Idea is to slit InputStream to string chunks and pass them to process function
 */
trait StreamProcessor {
  /**
   * Process stream implementation
   *
   * @param inputStream - input data stream
   * @param process - precess fuction
   */
  def consume(inputStream: InputStream, encoding: String = "UTF-8")(process: String => Unit): Unit

  /**
   * Name of stream processor
   * @return processor name
   */
  def name: String
}
