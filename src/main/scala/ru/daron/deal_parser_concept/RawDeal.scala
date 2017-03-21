package ru.daron.deal_parser_concept

/**
  * Created by Aleksei Terekhin on 16/03/2017.
  */
case class RawDeal(id: Option[String], title: Option[String], description: Option[String])

object RawDeal {
  lazy val empty = RawDeal(None, None, None)
}
