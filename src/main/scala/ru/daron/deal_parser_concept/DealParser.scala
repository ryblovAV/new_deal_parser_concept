package ru.daron.deal_parser_concept

trait DealParser {
  def parseF(config: Config)(in: Array[String]): RawDeal = {
    val f = config.combine _
    f(in)(RawDeal.empty)
  }
}

object DealParser extends DealParser
