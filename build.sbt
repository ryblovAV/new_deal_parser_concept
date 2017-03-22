name := "new_deal_parser_concept"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "com.typesafe.play" % "play-json_2.12" % "2.6.0-M5",
  "org.apache.commons" % "commons-lang3" % "3.5",
  "net.sf.supercsv" % "super-csv" % "2.4.0",
  "org.scalatest" % "scalatest_2.12" % "3.0.1" % "test"
)