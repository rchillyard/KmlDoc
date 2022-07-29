name := "KMLDoc"

version := "1.0"

//scalaVersion := "3.1.0"
scalaVersion := "2.13.8"

Compile / doc / scalacOptions ++= Seq("-Vimplicits", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused")
scalacOptions ++= Seq("-encoding", "UTF-8")


lazy val scalaModules = "org.scala-lang.modules"


// https://mvnrepository.com/artifact/org.scala-lang.modules/scala-xml
libraryDependencies += scalaModules %% "scala-xml" % "2.1.0"

libraryDependencies += "com.phasmidsoftware" % "flog_2.13" % "1.0.8"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.11" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "org.scalatest" %% "scalatest" % "3.2.12" % Test
)