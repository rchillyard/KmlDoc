package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.render.{FormatFree, FormatXML, Renderable, Renderers}
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
 * NOTE well: do not be tempted to add "_xmlns" as a member.
 * If you do, you will run into the undocumented(?) "feature" of the Xml library that "xmlns" is a reserved attribute name.
 *
 * @param Documents a sequence of Document.
 */
case class KML(Documents: Seq[Document]) {
  override def toString: String = KmlRenderers.rendererKml.render(this, 0)
}

case class Document(name: String, description: String, Styles: Seq[Style], StyleMaps: Seq[StyleMap], Folders: Seq[Folder])

case class Style()

case class StyleMap()

case class Folder(name: String, Placemarks: Seq[Placemark])

case class Placemark(name: String, maybedescription: Option[String], styleUrl: String, LineStrings: Seq[LineString])

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
  implicit val extractMaybeDescription: Extractor[Option[String]] = extractorOption[String]("junk")
  implicit val extractorStyle: Extractor[Style] = extractor0[Style](_ => Style()) // TODO flesh this out
  implicit val extractorStyleMap: Extractor[StyleMap] = extractor0[StyleMap](_ => StyleMap()) // TODO flesh this out
  implicit val extractorMultiString: MultiExtractor[Seq[String]] = multiExtractor[String]
  implicit val extractorMultiCoordinates: MultiExtractor[Seq[Coordinates]] = multiExtractor[Coordinates]
  implicit val extractorLineString: Extractor[LineString] = extractor11(LineString)
  implicit val extractorMultiLineString: MultiExtractor[Seq[LineString]] = multiExtractor[LineString]
  implicit val extractorPlacemark: Extractor[Placemark] = extractor31(Placemark)
  implicit val extractorMultiPlacemark: MultiExtractor[Seq[Placemark]] = multiExtractor[Placemark]
  implicit val extractorFolder: Extractor[Folder] = extractor11(Folder)
  implicit val extractorMultiStyleMap: MultiExtractor[Seq[StyleMap]] = multiExtractor[StyleMap]
  implicit val extractorMultiStyle: MultiExtractor[Seq[Style]] = multiExtractor[Style]
  implicit val extractorMultiFolder: MultiExtractor[Seq[Folder]] = multiExtractor[Folder]
  implicit val extractorDocument: Extractor[Document] = extractor23(Document)
  implicit val extractorMultiDocument: MultiExtractor[Seq[Document]] = multiExtractor[Document]
  implicit val extractorKml: Extractor[KML] = extractor01(KML)
  implicit val extractorMultiKml: MultiExtractor[Seq[KML]] = multiExtractor[KML]
}

object KmlRenderers extends Renderers {
  implicit val rendererOptionString: Renderable[Option[String]] = optionRenderer[String]
  implicit val rendererStyle: Renderable[Style] = renderer0(FormatFree)
  implicit val rendererStyleMap: Renderable[StyleMap] = renderer0(FormatFree)
  implicit val rendererCoordinate: Renderable[Coordinate] = renderer2(Coordinate.apply)
  implicit val rendererCoordinates1: Renderable[Seq[Coordinate]] = sequenceRenderer[Coordinate]
  implicit val rendererCoordinates: Renderable[Coordinates] = renderer1(Coordinates.apply)(FormatFree)
  implicit val rendererCoordinates_s: Renderable[Seq[Coordinates]] = sequenceRenderer[Coordinates]
  implicit val rendererLineString: Renderable[LineString] = renderer2(LineString)
  implicit val rendererLineStrings: Renderable[Seq[LineString]] = sequenceRenderer[LineString]
  implicit val rendererPlacemark: Renderable[Placemark] = renderer4(Placemark)
  implicit val rendererPlacemarks: Renderable[Seq[Placemark]] = sequenceRenderer[Placemark]
  implicit val rendererFolder: Renderable[Folder] = renderer2(Folder)
  implicit val rendererFolders: Renderable[Seq[Folder]] = sequenceRenderer[Folder]
  implicit val rendererStyles: Renderable[Seq[Style]] = sequenceRenderer[Style]
  implicit val rendererStyleMaps: Renderable[Seq[StyleMap]] = sequenceRenderer[StyleMap]
  implicit val rendererDocument: Renderable[Document] = renderer5(Document)
  implicit val rendererDocuments: Renderable[Seq[Document]] = sequenceRenderer[Document]
  implicit val rendererKml: Renderable[KML] = renderer1(KML)(FormatFree)
}

object KmlXmlRenderers extends Renderers {
  implicit val rendererOptionString: Renderable[Option[String]] = optionRenderer[String]
  implicit val rendererStyle: Renderable[Style] = renderer0(FormatXML)
  implicit val rendererStyleMap: Renderable[StyleMap] = renderer0(FormatXML)
  implicit val rendererCoordinate: Renderable[Coordinate] = renderer2(Coordinate.apply)
  implicit val rendererCoordinates1: Renderable[Seq[Coordinate]] = sequenceRenderer[Coordinate]
  implicit val rendererCoordinates: Renderable[Coordinates] = renderer1(Coordinates.apply)(FormatFree)
  implicit val rendererCoordinates_s: Renderable[Seq[Coordinates]] = sequenceRenderer[Coordinates]
  implicit val rendererLineString: Renderable[LineString] = renderer2(LineString)
  implicit val rendererLineStrings: Renderable[Seq[LineString]] = sequenceRenderer[LineString]
  implicit val rendererPlacemark: Renderable[Placemark] = renderer4(Placemark)
  implicit val rendererPlacemarks: Renderable[Seq[Placemark]] = sequenceRenderer[Placemark]
  implicit val rendererFolder: Renderable[Folder] = renderer2(Folder)
  implicit val rendererFolders: Renderable[Seq[Folder]] = sequenceRenderer[Folder]
  implicit val rendererStyles: Renderable[Seq[Style]] = sequenceRenderer[Style]
  implicit val rendererStyleMaps: Renderable[Seq[StyleMap]] = sequenceRenderer[StyleMap]
  implicit val rendererDocument: Renderable[Document] = renderer5(Document)
  implicit val rendererDocuments: Renderable[Seq[Document]] = sequenceRenderer[Document]
  implicit val rendererKml: Renderable[KML] = renderer1(KML)(FormatFree)
}

object KMLCompanion {

  // TESTME
  def loadKML(resource: URL): KML = {
    require(resource != null)
    loadKML(resource.getPath)
  }

  // TESTME
  def loadKML(file: String): KML = {
    require(file != null)
    val xml: Elem = XML.loadFile(file)
    //    val kmls: collection.Seq[KML] = for (kml <- xml \\ "kml") yield KML.fromXML(kml)
    //    kmls.head
    KML(Nil)
  }
}

object Test extends App {
  // TESTME
  val kml: KML = KMLCompanion.loadKML(KML.getClass.getResource("sample.kml"))
  println(s"KML: $kml")
}
