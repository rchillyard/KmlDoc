package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.xml.{Extractor, Extractors, XmlException}

import java.net.URL
import scala.io.Source
import scala.util.Success
import scala.util.matching.Regex
import scala.xml.{Elem, Node, XML}

/**
 * Case class to define a KML object.
 *
 * TODO allow some of the members (not just in KML) to be optional.
 *
 * @param _xmlns the xmlns value.
 * @param documents a sequence of Document.
 */
case class KML(_xmlns: String, documents: Seq[Document])

case class Document(name: String, description: String, Styles: Seq[Style], StyleMaps: Seq[StyleMap], Folders: Seq[Folder])

case class Style()

case class StyleMap()

case class Folder(name: String, Placemarks: Seq[Placemark])

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

object KmlExtractors extends Extractors {

  import Extractors._

  implicit val extractorCoordinates: Extractor[Coordinates] =
    (node: Node) => Success(Coordinates.parse(node.text))
  implicit val extractorCoordinatesSequence: Extractor[Seq[Coordinates]] =
    extractorSequence[Coordinates]("coordinates")
  implicit val extractorLineString: Extractor[LineString] =
    extractor2(LineString)
  implicit val extractorLineStringSequence: Extractor[Seq[LineString]] =
    extractorSequence[LineString]("LineString")
  implicit val extractorPlacemark: Extractor[Placemark] =
    extractor4(Placemark)
  implicit val extractorPlacemarkSequence: Extractor[Seq[Placemark]] =
    extractorSequence[Placemark]("Placemark")
  implicit val extractorFolder: Extractor[Folder] =
    extractor2(Folder)
  implicit val extractorFolderSequence: Extractor[Seq[Folder]] =
    extractorSequence[Folder]("Folder")

  implicit val extractMaybeDescription: Extractor[Option[String]] = extractorOption[String]("junk")

  implicit val extractorStyle: Extractor[Style] =
    extractor0[Style](_ => Style()) // TODO flesh this out
  implicit val extractorStyleSequence: Extractor[Seq[Style]] =
    extractorSequence[Style]("Style")
  implicit val extractorStyleMap: Extractor[StyleMap] =
    extractor0[StyleMap](_ => StyleMap()) // TODO flesh this out
  implicit val extractorStyleMapSequence: Extractor[Seq[StyleMap]] =
    extractorSequence[StyleMap]("StyleMap")
  implicit val extractorDocument: Extractor[Document] =
    extractor5(Document)
  implicit val extractorDocumentSequence: Extractor[Seq[Document]] =
    extractorSequence[Document]("Document")

  implicit val extractorKml: Extractor[KML] =
    extractor2(KML)
  implicit val extractorKmlSequence: Extractor[Seq[KML]] =
    extractorSequence[KML]("kml")
}


object KMLCompanion {

  def loadKML(resource: URL): KML = {
    require(resource != null)
    loadKML(resource.getPath)
  }

  def loadKML(file: String): KML = {
    require(file != null)
    val xml: Elem = XML.loadFile(file)
    //    val kmls: collection.Seq[KML] = for (kml <- xml \\ "kml") yield KML.fromXML(kml)
    //    kmls.head
    KML("", Nil)
  }
}



object Test extends App {
  val kml: KML = KMLCompanion.loadKML(KML.getClass.getResource("sample.kml"))
  println(s"KML: $kml")
}