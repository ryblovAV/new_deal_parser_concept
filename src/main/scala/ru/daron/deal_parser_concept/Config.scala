package ru.daron.deal_parser_concept

/**
  * Created by Aleksei Terekhin on 16/03/2017.
  */
final case class Config(rules: Seq[ParseRule]) {
  def combine(in: Array[String]): RawDeal => RawDeal =
    rules.foldLeft(identity _ : RawDeal => RawDeal)((acc, f) => acc andThen f.process(in))
}
