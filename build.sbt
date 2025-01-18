organization := "com.phasmidsoftware"

name := "KMLDoc"

version := "1.0.4"

scalaVersion := "2.13.16"

Compile / doc / scalacOptions ++= Seq("-explaintypes", "-Vimplicits", "-implicits-debug", "-implicits-show-all", "-unchecked", "-feature", "-Xcheckinit", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused", "-deprecation")

lazy val scalaModules = "org.scala-lang.modules"

libraryDependencies += scalaModules %% "scala-xml" % "2.3.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.7"

libraryDependencies ++= Seq(
  "com.phasmidsoftware" %% "flog" % "1.0.9",
  "com.phasmidsoftware" %% "args" % "1.0.3",
  "ch.qos.logback" % "logback-classic" % "1.5.16" % "runtime",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)