package ru.daron.deal_parser_concept

import org.joda.time.DateTime


case class Tag(name: String)
case class AddInfo(numericFields: Map[String, Double], booleanFields: Map[String, Boolean], textFields: Map[String, String], listFields: Map[String, List[String]], dateFields: Map[String, DateTime] = Map.empty[String, DateTime])
object AddInfo {
  val empty = AddInfo(Map(), Map(), Map(), Map(), Map())
}

case class RawTokens(single: Map[String, String], multi: Map[String, List[String]])
object RawTokens {
  val empty = RawTokens(Map.empty, Map.empty)
}

case class RawDeal(id: String,
                   title: Option[String],
                   description: Option[String],
                   tags: List[Tag],
                   addInfo: AddInfo = AddInfo.empty,
                   isActive: Boolean = true,
                   rawTokens: RawTokens = RawTokens.empty)

object RawDeal {
  lazy val empty = RawDeal("", None, None, Nil)
}
