package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.core.WithSuper
import com.phasmidsoftware.render._
import com.phasmidsoftware.xml._
import java.net.URL
import org.slf4j.{Logger, LoggerFactory}
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Using}
import scala.xml.{Elem, NamespaceBinding, Node, XML}

/**
 * Class which is the supertype of all KML types.
 *
 * For more details, see https://developers.google.com/kml/documentation/kmlreference
 */


case class KMLObject(_id: String = "")
//  val _targetID: String

case class Feature(name: String)(val base: KMLObject)

/**
 * Supertype of PhotoOverlay, ScreenOverlay, GroundOverlay.
 */
case class OverlayBase()(val feature: Feature)

/**
 * Supertype of Folder, Document.
 */
sealed trait ContainerBase

case class Container(properties: Feature)

/**
 * Supertype of Point, LineString, LinearRing, Polygon, MultiGeometry, Model.
 */
sealed trait GeometryBase

/**
 * Supertype of Style and StyleMap.
 */
class StyleSelector(styleSelectorProperties: StyleSelectorProperties)

case class StyleSelectorProperties(unit: Unit)(val kmlObject: KMLObject)


/**
 * Supertype of TimeSpan and TimeStamp.
 */
class TimePrimitive(timePrimitive: TimePrimitiveProperties)

case class TimePrimitiveProperties()(val kmlObject: KMLObject)

/**
 * Supertype of Camera and Lookat.
 */
//sealed trait AbstractViewBase extends KMLBase


/**
 * Supertype of ColorStyleBase, ListStyle and BalloonStyle.
 */
//sealed trait SubStyleBase extends KMLBase

/**
 * Supertype of LineStyle, PolyStyle, IconStyle and LabelStyle.
 */
//sealed trait ColorStyleBase extends SubStyleBase

/**
 * Case class to define a KML object.
 *
 * For a reference document for KML objects, please see https://developers.google.com/kml/documentation/kmlreference
 *
 * NOTE WELL: do not be tempted to add "_xmlns" as a member.
 * If you do, you will run into the undocumented(?) "feature" of the Xml library that "xmlns" is a reserved attribute name.
 *
 * @param Documents a sequence of Document.
 */
// TODO add in the xmlns tag (a top-level attribute)
case class KML(Documents: Seq[Document])

case class KML_Binding(kml: KML, binding: NamespaceBinding)

/**
 * Case class to represent a Location.
 *
 * @param longitude the longitude.
 * @param latitude the latitued.
 * @param altitude the altitude in meters.
 */
case class Location(longitude: Double, latitude: Double, altitude: Int)(val kmlObject: KMLObject)

/**
 * Case class to represent a Document.
 *
 * CONSIDER what is the difference between name: Text and, for example, Scale($: String).
 *
 * @param name        an XML element of the form: <name>the name</name>.
 * @param description an XML element of the form: <description>the description</description>.
 * @param Styles      a sequence of Style or StyleMap elements.
 * @param Folders     a sequence of Folder elements.
 */
case class Document(properties: Container, name: Text, maybeOpen: Option[Int], description: Text, Styles: Seq[StyleSelector], Folders: Seq[Folder]) extends ContainerBase

/**
 * Case class to represent a Scale which is represented in XML as, for example: <scale>1.1</scale>
 *
 * @param $ the value of the scale (a Double).
 */
case class Scale($: Double)

case class Icon(href: Text)

case class HotSpot(_x: Int, _xunits: String, _y: Int, _yunits: String)

case class IconStyle(scale: Scale, Icon: Icon, hotSpot: HotSpot)
//case class IconStyle(maybeScale: Option[Scale], Icon: Icon, hotSpot: HotSpot)

case class LabelStyle(scale: Scale)

case class BalloonStyle(text: Text)

case class Color($: String)

case class Width($: Double)

case class LineStyle(color: Color, width: Width)

/**
 * Style element.
 * It seems there are two completely different types of Style element, but they are not distinguished.
 * Type A has IconStyle, LabelStyle, BalloonStyle;
 * Type B has LineStyle.
 *
 * FIXME need to render Style (and StyleMap) correctly.
 *
 * @param _id               the identifier of the Style.
 * @param maybeIconStyle    the icon style (optional)
 * @param maybeLabelStyle   the label style (optional)
 * @param maybeBalloonStyle the balloon style (optional)
 * @param maybeLineStyle    the line style (optional)
 */
case class Style(maybeIconStyle: Option[IconStyle])(val superObject: StyleSelectorProperties)
        extends StyleSelector(superObject) with WithSuper[Style, StyleSelectorProperties]
//case class Style(maybeIconStyle: Option[IconStyle], maybeLabelStyle: Option[LabelStyle], maybeBalloonStyle: Option[BalloonStyle], maybeLineStyle: Option[LineStyle])(val superObject: StyleSelectorProperties)
//        extends StyleSelector(superObject) with WithSuper[Style, StyleSelectorProperties]

case class Pair(key: String, styleUrl: String)

case class StyleMap(Pairs: Seq[Pair])(val styleSelectorProperties: StyleSelectorProperties) extends StyleSelector(styleSelectorProperties)

case class Folder(name: Text, Placemarks: Seq[Placemark])

case class Placemark(name: Text, maybedescription: Option[Text], styleUrl: Text, LineStrings: Seq[LineString], Points: Seq[Point])

case class Tessellate($: String)

case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates])

case class Point(coordinates: Seq[Coordinates])

case class Coordinates(coordinates: Seq[Coordinate])

object Coordinates {
  def parse(w: String): Coordinates = {
    val source = Source.fromString(w)
    Coordinates((for (line <- source.getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
  }
}

case class Coordinate(lat: String, long: String, alt: String)

object Coordinate {
  val latLong: Regex = """\s*([\d\-\.]+),([\d\-\.]+),([\d\-\.]+)""".r

  def apply(w: String): Coordinate = w match {
    case latLong(long, lat, alt) => Coordinate(lat, long, alt)
    case _ => throw XmlException(s"bad coordinate string: $w")
  }
}

object KmlExtractors extends Extractors {

  Extractors.translations += "coordinates" -> "coordinates"

  import Extractors._

  implicit val extractorKMLObject: Extractor[KMLObject] = extractor10(KMLObject)
  implicit val extractorCoordinates: Extractor[Coordinates] = (node: Node) => Success(Coordinates.parse(node.text))
  implicit val extractorScale: Extractor[Scale] = extractor10(Scale)
  //  implicit val extractMaybeScale: Extractor[Option[Scale]] = extractorOption
  implicit val extractorIcon: Extractor[Icon] = extractor10(Icon)
  implicit val extractorColor: Extractor[Color] = extractor10(Color)
  implicit val extractorWidth: Extractor[Width] = extractor10(Width)
  implicit val extractorHotspot: Extractor[HotSpot] = extractor40(HotSpot)
  implicit val extractorIconStyle: Extractor[IconStyle] = extractor30(IconStyle)
  implicit val extractorBalloonStyle: Extractor[BalloonStyle] = extractor10(BalloonStyle)
  implicit val extractorLabelStyle: Extractor[LabelStyle] = extractor10(LabelStyle)
  implicit val extractorLineStyle: Extractor[LineStyle] = extractor20(LineStyle)
  implicit val extractMaybeIconStyle: Extractor[Option[IconStyle]] = extractorOption
  implicit val extractMaybeLabelStyle: Extractor[Option[LabelStyle]] = extractorOption
  implicit val extractMaybeBalloonStyle: Extractor[Option[BalloonStyle]] = extractorOption
  implicit val extractMaybeLineStyle: Extractor[Option[LineStyle]] = extractorOption
  implicit val extractorStyleSelectorProperties: Extractor[StyleSelectorProperties] = extractorPartial[KMLObject, StyleSelectorProperties](extractorPartial10(StyleSelectorProperties.apply))
  implicit val extractorPair: Extractor[Pair] = extractor20(Pair)
  implicit val extractorMultiPair: MultiExtractor[Seq[Pair]] = multiExtractor[Pair]
  implicit val extractorStyle: Extractor[Style] = extractorPartial[StyleSelectorProperties, Style](extractorPartial10(Style.apply))
  implicit val extractorStyleMap: Extractor[StyleMap] = extractorPartial[StyleSelectorProperties, StyleMap](extractorPartial01(StyleMap.apply))
//    extractorSuper[StyleSelectorProperties, StyleMap](extractor01Super(StyleMap, true))
  implicit val extractorStyleType: Extractor[StyleSelector] = extractorAlt[StyleSelector, Style, StyleMap]
  implicit val extractorMultiCoordinates: MultiExtractor[Seq[Coordinates]] = multiExtractor[Coordinates]
  implicit val extractorTessellate: Extractor[Tessellate] = extractor10(Tessellate)
  implicit val extractorLineString: Extractor[LineString] = extractor11(LineString)
  implicit val extractorMultiLineString: MultiExtractor[Seq[LineString]] = multiExtractor[LineString]
  implicit val extractorPoint: Extractor[Point] = extractor01(Point)
  implicit val extractorMultiPoint: MultiExtractor[Seq[Point]] = multiExtractor[Point]
  implicit val extractorPlacemark: Extractor[Placemark] = extractor32(Placemark)
  implicit val extractorMultiPlacemark: MultiExtractor[Seq[Placemark]] = multiExtractor[Placemark]
  implicit val extractorFolder: Extractor[Folder] = extractor11(Folder)
  implicit val extractorMultiStyleMap: MultiExtractor[Seq[StyleMap]] = multiExtractor[StyleMap]
  implicit val extractorMultiStyle: MultiExtractor[Seq[StyleSelector]] = multiExtractor[StyleSelector]
  implicit val extractorMultiFolder: MultiExtractor[Seq[Folder]] = multiExtractor[Folder]
  implicit val extractMaybeOpen: Extractor[Option[Int]] = extractorOption
  implicit val extractFeature: Extractor[Feature] = extractor10(Feature)
  implicit val extractContainer: Extractor[Container] = extractor10(Container)
  implicit val extractorDocument: Extractor[Document] = extractor42(Document)
  implicit val extractorMultiDocument: MultiExtractor[Seq[Document]] = multiExtractor[Document]
  implicit val extractorKml: Extractor[KML] = extractor01(KML)
  implicit val extractorMultiKml: MultiExtractor[Seq[KML]] = multiExtractor[KML]
}

trait KmlRenderers extends Renderers {

  case class FormatCoordinate(indents: Int) extends BaseFormat(indents) {
    val name: String = "formatCoordinate"

    def indent: Format = copy(indents = indents + 1)

    def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = open match {
      case Some(true) => newline
      case _ => ""
    }

    def sequencer(open: Option[Boolean]): String = open match {
      case Some(false) => ""
      case _ => newline
    }
  }

  import Renderers._

  implicit val rendererScale: Renderable[Scale] = renderer1(Scale)
  //  implicit val rendererOptionScale: Renderable[Option[Scale]] = optionRenderer
  implicit val rendererIcon: Renderable[Icon] = renderer1(Icon)
  implicit val rendererColor: Renderable[Color] = renderer1(Color)
  implicit val rendererWidth: Renderable[Width] = renderer1(Width)
  implicit val rendererHotSpot: Renderable[HotSpot] = renderer4(HotSpot)
  implicit val rendererIconStyle: Renderable[IconStyle] = renderer3(IconStyle)
  implicit val rendererBalloonStyle: Renderable[BalloonStyle] = renderer1(BalloonStyle)
  implicit val rendererLabelStyle: Renderable[LabelStyle] = renderer1(LabelStyle)
  implicit val rendererLineStyle: Renderable[LineStyle] = renderer2(LineStyle)
  implicit val rendererOptionLineStyle: Renderable[Option[LineStyle]] = optionRenderer
  implicit val rendererOptionLabelStyle: Renderable[Option[LabelStyle]] = optionRenderer
  implicit val rendererOptionBalloonStyle: Renderable[Option[BalloonStyle]] = optionRenderer
  implicit val rendererOptionIconStyle: Renderable[Option[IconStyle]] = optionRenderer
  implicit val rendererStyle: Renderable[Style] = renderer5(Style)
  implicit val rendererPair: Renderable[Pair] = renderer2(Pair)
  implicit val rendererSequencePair: Renderable[Seq[Pair]] = sequenceRenderer[Pair]
  implicit val rendererStyleMap: Renderable[StyleMap] = renderer2(StyleMap)
  implicit val rendererCoordinate: Renderable[Coordinate] = (t: Coordinate, _: Format, _: StateR) => s"${t.long}, ${t.lat}, ${t.alt}"
  implicit val rendererCoordinates1: Renderable[Seq[Coordinate]] = sequenceRendererFormatted[Coordinate](FormatCoordinate)
  implicit val rendererCoordinates: Renderable[Coordinates] = renderer1(Coordinates.apply)
  // TODO refactor the sequenceRendererFormatted method so that its parameter is a Format=>Format function.
  implicit val rendererCoordinates_s: Renderable[Seq[Coordinates]] = sequenceRendererFormatted[Coordinates](FormatXML)
  implicit val rendererTessellate: Renderable[Tessellate] = renderer1(Tessellate)
  implicit val rendererLineString: Renderable[LineString] = renderer2(LineString)
  implicit val rendererLineStrings: Renderable[Seq[LineString]] = sequenceRenderer[LineString]
  implicit val rendererPoint: Renderable[Point] = renderer1(Point)
  implicit val rendererPoints: Renderable[Seq[Point]] = sequenceRenderer[Point]
  implicit val rendererPlacemark: Renderable[Placemark] = renderer5(Placemark)
  implicit val rendererPlacemarks: Renderable[Seq[Placemark]] = sequenceRenderer[Placemark]
  implicit val rendererFolder: Renderable[Folder] = renderer2(Folder)
  implicit val rendererFolders: Renderable[Seq[Folder]] = sequenceRenderer[Folder]
  implicit val rendererStyles: Renderable[Seq[Style]] = sequenceRenderer[Style]
  implicit val rendererStyleMaps: Renderable[Seq[StyleMap]] = sequenceRenderer[StyleMap]
  implicit val rendererStyleType: Renderable[StyleSelectorBase] = altRenderer[StyleSelectorBase, Style, StyleMap]
  implicit val rendererStyleTypes: Renderable[Seq[StyleSelectorBase]] = sequenceRenderer[StyleSelectorBase]
  implicit val rendererOptionOpen: Renderable[Option[Int]] = optionRenderer
  implicit val rendererObject: Renderable[KMLObject] = renderer1(KMLObject)
  implicit val rendererFeature: Renderable[Feature] = renderer1(Feature)
  implicit val rendererContainer: Renderable[Container] = renderer1(Container)
  implicit val rendererDocument: Renderable[Document] = renderer6(Document)
  implicit val rendererDocuments: Renderable[Seq[Document]] = sequenceRenderer[Document]
  implicit val rendererKml: Renderable[KML] = renderer1(KML)
  implicit val rendererKml_Binding: Renderable[KML_Binding] = (t: KML_Binding, format: Format, stateR: StateR) =>
    doRenderKML_Binding(t, format, stateR)

  private def doRenderKML_Binding(t: KML_Binding, format: Format, stateR: StateR): String = {
    val renderer: Renderable[KML] = renderer1(KML)
    val wy = Using(stateR.addAttribute(s"""${t.binding}"""))(rs => renderer.render(t.kml, format, rs))
    wy match {
      case Success(w) => w
      case Failure(x) => logger.warn("doRenderKML_Binding", x); ""
    }
  }
}

// CONSIDER Rename as KML
object KMLCompanion {

  val logger: Logger = LoggerFactory.getLogger(KML.getClass)

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

