package ru.daron.deal_parser_concept

import org.joda.time.DateTime


case class Tag(name: String)

case class AddInfo(numericFields: Map[String, Double],
                   booleanFields: Map[String, Boolean],
                   textFields: Map[String, String],
                   listFields: Map[String, List[String]],
                   dateFields: Map[String, DateTime])

object AddInfo {
  val empty = AddInfo(Map(), Map(), Map(), Map(), Map())
}

case class RawTokens(single: Map[String, String], multi: Map[String, List[String]]) {
  def merge(other: RawTokens): RawTokens = RawTokens(
    mergeMaps(this.single, other.single)((v1, v2) => v1),
    mergeMaps(this.multi, other.multi)((v1, v2) => (v1 ++ v2).distinct)
  )

  def mergeMaps[K, V](m1: Map[K, V], m2: Map[K, V])(merge: (V, V) => V): Map[K, V] = {
    m1.foldLeft(m2){ case (acc, (key, value)) =>
      acc.get(key) match {
        case None => acc.updated(key, value)
        case Some(existingValue) => acc.updated(key, merge(value, existingValue))
      }
    }
  }
}

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
