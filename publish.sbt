ThisBuild / organization := "com.phasmidsoftware"
ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rchillyard/KmlDoc"),
    "scm:git@github.com:rchillyard/KmlDoc.git"
  )
  )
  ThisBuild / developers := List(
  Developer(
  id    = "rchillyard",
  name  = "Robin Hillyard",
  email = "rchillyard@phasmidsoftware.com",
  url   = url("https://phasmidsoftware.com")
  )
  )

  ThisBuild / description := "This project is a Scala library for reading, writing, and manipulating KML files."
  ThisBuild / licenses := List("Apache-2.0" -> new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
  ThisBuild / homepage := Some(url("https://github.com/rchillyard/KmlDoc"))

  // Remove all additional repository other than Maven Central from POM
  ThisBuild / pomIncludeRepository := { _ => false }
  ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
  ThisBuild / publishMavenStyle := true