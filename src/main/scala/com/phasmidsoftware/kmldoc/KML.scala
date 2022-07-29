package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.xml.XmlException

import java.net.URL
import scala.io.Source
import scala.util.matching.Regex
import scala.xml.{Elem, XML}

case class KML(_xmlns: Option[String], documents: Seq[Document])

case class Document(name: String, description: Option[String], folders: Seq[Folder])

case class Folder(name: String, placemarks: Seq[Placemark])

case class Placemark(name: String, description: String, styleUrl: String, LineStrings: Seq[LineString])

case class LineString(tessellate: String, coordinates: Seq[Coordinates])

case class Coordinates(coordinates: Seq[Coordinate])

object Coordinates {
  def parse(w: String): Coordinates = {
    val source = Source.fromString(w)
    Coordinates((for (line <- source.getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
  }
}

case class Coordinate(lat: String, long: String)

object Coordinate {
  val latLong: Regex = """\s*([\d\-\.]+),([\d\-\.]+),([\d\-\.]+)""".r

  def apply(w: String): Coordinate = w match {
    case latLong(long, lat, _) => Coordinate(lat, long)
    case _ => throw XmlException(s"bad coordinate string: $w")
  }
}

object KML {

  def loadKML(resource: URL): KML = {
    require(resource != null)
    loadKML(resource.getPath)
  }

  def loadKML(file: String): KML = {
    require(file != null)
    val xml: Elem = XML.loadFile(file)
    //    val kmls: collection.Seq[KML] = for (kml <- xml \\ "kml") yield KML.fromXML(kml)
    //    kmls.head
    KML(None, Nil)
  }
}



object Test extends App {
  val kml: KML = KML.loadKML(KML.getClass.getResource("sample.kml"))
  println(s"KML: $kml")
}