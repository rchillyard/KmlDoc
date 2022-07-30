package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.xml.{Extractor, Extractors, MultiExtractor, XmlException}

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

case class LineString(tessellate: String, Coordinates: Seq[Coordinates])

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

  Extractors.translations += "Coordinates" -> "coordinates"

  import Extractors._

  implicit val extractorCoordinates: Extractor[Coordinates] = (node: Node) => Success(Coordinates.parse(node.text))
  //  implicit val extractorCoordinatesSequence: Extractor[Seq[Coordinates]] = extractorSequence[Coordinates]("coordinates")
  //  implicit val extractorLineStringSequence: Extractor[Seq[LineString]] = extractorSequence[LineString]("LineString")
  //  implicit val extractorPlacemarkSequence: Extractor[Seq[Placemark]] = extractorSequence[Placemark]("Placemark")
  //  implicit val extractorFolderSequence: Extractor[Seq[Folder]] = extractorSequence[Folder]("Folder")

  implicit val extractMaybeDescription: Extractor[Option[String]] = extractorOption[String]("junk")

  implicit val extractorStyle: Extractor[Style] =
    extractor0[Style](_ => Style()) // TODO flesh this out
  implicit val extractorStyleSequence: Extractor[Seq[Style]] =
    extractorSequence[Style]("Style")
  implicit val extractorStyleMap: Extractor[StyleMap] =
    extractor0[StyleMap](_ => StyleMap()) // TODO flesh this out
  implicit val extractorStyleMapSequence: Extractor[Seq[StyleMap]] =
    extractorSequence[StyleMap]("StyleMap")
  //  implicit val extractorDocumentSequence: Extractor[Seq[Document]] = extractorSequence[Document]("Document")
  implicit val poo1: MultiExtractor[Seq[String]] = new MultiExtractorBase[String]()
  implicit val poo2: MultiExtractor[Seq[Coordinates]] = new MultiExtractorBase[Coordinates]()
  implicit val extractorLineString: Extractor[LineString] = extractor11(LineString)
  implicit val poo3: MultiExtractor[Seq[LineString]] = new MultiExtractorBase[LineString]()
  implicit val extractorPlacemark: Extractor[Placemark] = extractor31(Placemark)
  implicit val poo4: MultiExtractor[Seq[Placemark]] = new MultiExtractorBase[Placemark]()
  implicit val extractorFolder: Extractor[Folder] = extractor11(Folder)
  implicit val poo5: MultiExtractor[Seq[StyleMap]] = new MultiExtractorBase[StyleMap]()
  implicit val poo6: MultiExtractor[Seq[Style]] = new MultiExtractorBase[Style]()
  implicit val poo7: MultiExtractor[Seq[Folder]] = new MultiExtractorBase[Folder]()
  implicit val extractorDocument: Extractor[Document] = extractor23(Document)
  implicit val poo8: MultiExtractor[Seq[Document]] = new MultiExtractorBase[Document]()

  implicit val extractorKml: Extractor[KML] =
    extractor11(KML)
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