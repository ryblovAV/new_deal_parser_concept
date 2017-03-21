package ru.daron.deal_parser_concept

/**
  * Created by Aleksei Terekhin on 16/03/2017.
  */
object ParserApp extends App {

  val inRawData =
    """01,Title1,Desc1
      |02,Title2,Desc2
      |03,Title3,Desc3
      |04,Title4,Desc4
      |05,Title5,Desc5
      |06,Title6,Desc6
      |07,Title7,Desc7
      |08,Title8,Desc8
      |09,Title9,Desc9""".stripMargin

  println(s"RawInputExample: \n$inRawData")

  val config = Config(rules = Seq(
    ParseId(0),
    ParseTitle(1),
    ParseDescription(2)
  ))

  println(s"Example config: ${config}")
  val in: Seq[Array[String]] = inRawData.split("\n").map(_.split(","))
  val parseF: Array[String] => RawDeal = DealParser.parseF(config)

  val rawDeals = in.map(parseF)
  println(s"Result will be: \n${rawDeals}")
}
