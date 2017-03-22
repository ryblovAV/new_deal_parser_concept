package ru.daron.deal_parser_concept

sealed trait ParseRule {
  val index: Int

  def parse(in: Array[String]): Option[String] = {
    if (index > in.length - 1 || index < 0) { None } else Option(in(index))
  }

  def process(in: Array[String]): RawDeal => RawDeal
}

final case class ParseTitle(index: Int) extends ParseRule {
  override def process(in: Array[String]): RawDeal => RawDeal = { rawDeal =>
    val title = parse(in)
    rawDeal.copy(title = title)
  }
}

final case class ParseDescription(index: Int) extends ParseRule {
  override def process(in: Array[String]): RawDeal => RawDeal = { rawDeal =>
    val desc = parse(in)
    rawDeal.copy(description = desc)
  }
}

final case class ParseId(index: Int) extends ParseRule {
  override def process(in: Array[String]): RawDeal => RawDeal = { rawDeal =>
    val id = parse(in)
    rawDeal.copy(id = id.get)
  }
}





