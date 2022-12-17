package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.core.{Text, XmlException}
import com.phasmidsoftware.render._
import com.phasmidsoftware.xml.Extractor.none
import com.phasmidsoftware.xml._
import java.net.URL
import org.slf4j.{Logger, LoggerFactory}
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Using}
import scala.xml.{Elem, NamespaceBinding, Node, XML}

/**
 * Super-type of all KML entities.
 * See https://developers.google.com/kml/documentation/kmlreference
 */
class KmlObject

object KmlObject {
//  implicit object loggableKmlObject extends LoggableAny[KmlObject]
}

/**
 * Properties of KMLObject
 *
 * @param __id an optional identifier.
 */
case class KmlData(__id: Option[String])

object KmlData {
  def nemo: KmlData = KmlData(None)

//  implicit object loggableKmlData extends LoggableAny[KmlData]
}

/**
 * Case class to represent a Scale which is represented in XML as, for example: <scale>1.1</scale>
 *
 * @param $ the value of the scale (a Double).
 */
case class Scale($: Double)(val kmlData: KmlData) extends KmlObject

object Scale {
  def nemo(x: Double): Scale = new Scale(x)(KmlData.nemo)
}

class Feature extends KmlObject

case class FeatureData(name: Text, maybeDescription: Option[Text], maybeStyleUrl: Option[String], maybeOpen: Option[Int], StyleSelectors: Seq[StyleSelector])(val kmlData: KmlData)

case class Placemark(Geometry: Seq[Geometry])(val featureData: FeatureData) extends Feature

class Container() extends Feature

object Container {
  val applyFunction: Unit => Container = _ => new Container()
}

case class ContainerData(featureData: FeatureData)

object ContainerData {
  val applyFunction: FeatureData => ContainerData = new ContainerData(_)
}

case class Folder(features: Seq[Feature])(val containerData: ContainerData) extends Container

/**
 * Document.
 *
 * TODO add Schemas to this case class
 *
 * @param features      a sequence of Features.
 * @param containerData ContainerData.
 */
case class Document(features: Seq[Feature])(val containerData: ContainerData) extends Container

class Geometry extends KmlObject

case class GeometryData(kmlData: KmlData)

object GeometryData {
  val applyFunction: KmlData => GeometryData = new GeometryData(_)
}

case class Point(coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry

/**
 * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
 */
class StyleSelector() extends KmlObject

case class StyleSelectorData(kmlData: KmlData)

object StyleSelectorData {
  val applyFunction: KmlData => StyleSelectorData = new StyleSelectorData(_)
}

/**
 * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
 */
class SubStyle() extends KmlObject

case class SubStyleData(kmlData: KmlData)

object SubStyleData {
  val applyFunction: KmlData => SubStyleData = new SubStyleData(_)
}

class ColorStyle() extends SubStyle

case class ColorStyleData(color: Color, maybeColorMode: Option[ColorMode])(val subStyleData: SubStyleData)

/**
 * BalloonStyle
 *
 * NOTE Use of the color element has been deprecated (use bgColor instead.)
 * NOTE According to the current KML reference, this object extends SubStyle or ColorStyle (it's not clear which).
 *
 * @param text           the balloon text.
 * @param maybeBgColor   optional background color (maybe it isn't optional if there's no color element).
 * @param maybeTextColor optional text color.
 * @param displayMode    the display mode.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class BalloonStyle(text: Text, maybeBgColor: Option[BgColor], maybeTextColor: Option[TextColor], displayMode: DisplayMode)(val colorStyleData: ColorStyleData) extends ColorStyle

/**
 * ListStyle
 *
 * NOTE According to the current KML reference, this object extends SubStyle or ColorStyle (it's not clear which).
 *
 * @param bgColor           optional background color (maybe it isn't optional if there's no color element).
 * @param maybeListItemType optional ListItemType.
 * @param maybeItemIcon     the display mode.
 * @param colorStyleData    the (auxiliary) color style properties.
 */
case class ListStyle(bgColor: BgColor, maybeListItemType: Option[ListItemType], maybeItemIcon: Option[ItemIcon])(val colorStyleData: ColorStyleData) extends ColorStyle

/**
 * LineStyle
 *
 * @param width          the line width
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class LineStyle(width: Width)(val colorStyleData: ColorStyleData) extends ColorStyle

/**
 * PolyStyle
 *
 * @param fill           the value of fill.
 * @param outline        the value of outline.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class PolyStyle(fill: Fill, outline: Outline)(val colorStyleData: ColorStyleData) extends ColorStyle

/**
 * LabelStyle
 *
 * @param scale          the scale.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class LabelStyle(scale: Scale)(val colorStyleData: ColorStyleData) extends ColorStyle

/**
 * IconStyle
 *
 * @param scale          the Scale.
 * @param Icon           the Icon.
 * @param hotSpot        the HotSpot
 * @param maybeHeading   an optional Heading.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class IconStyle(scale: Scale, Icon: Icon, hotSpot: HotSpot, maybeHeading: Option[Heading])(val colorStyleData: ColorStyleData) extends ColorStyle

/**
 * Fill.
 * NOTE that this is a boolean that is represented by 0 or 1.
 *
 * @param boolean whether to fill or not.
 */
case class Fill(boolean: Int)

/**
 * Outline.
 * NOTE that this is a boolean that is represented by 0 or 1.
 *
 * @param boolean whether to outline or not.
 */
case class Outline(boolean: Int)

/**
 * Case class to represent a Heading which is represented in XML as, for example: <heading>1.1</heading>
 *
 * @param $ the value of the heading (a Double).
 */
case class Heading($: Double)

case class BgColor($: String)

/**
 * TextColor
 * Used by BalloonStyle.
 *
 * @param $ the color.
 */
case class TextColor($: String)

/**
 * DisplayMode which has values "default" or "hide".
 * Used by BalloonStyle.
 *
 * @param $ the mode.
 */
case class DisplayMode($: String)

/**
 * ListItemType
 * TODO this should be an enumerated type with values: check,checkOffOnly,checkHideChildren,radioFolder
 *
 * @param $ the value.
 */
case class ListItemType($: String)

/**
 * State
 * TODO this should be an enumerated type with values: open, closed, error, fetching0, fetching1, or fetching2
 *
 * @param $ the value.
 */
case class State($: String)

/**
 * ItemIcon
 *
 * @param state the state.
 */
case class ItemIcon(state: State, href: Text)

case class Icon(href: Text)

case class HotSpot(_x: Int, _xunits: String, _y: Int, _yunits: String)

case class Color($: String)

case class ColorMode($: String)

case class Width($: Double)

/**
 * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
 */

/**
 * Case class to define a KML object.
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
 * Style element.
 * It seems there are two completely different types of Style element, but they are not distinguished.
 * Type A has IconStyle, LabelStyle, BalloonStyle;
 * Type B has LineStyle.
 *
 * FIXME need to render Style (and StyleMap) correctly.
 *
 * @param Styles a set of different types of Style.
 *               CONSIDER constraining this set to be distinct.
 */
case class Style(Styles: Seq[ColorStyle])(val styleSelectorData: StyleSelectorData) extends StyleSelector

case class Pair(key: String, styleUrl: String)

case class StyleMap(Pairs: Seq[Pair])(val styleSelectorData: StyleSelectorData) extends StyleSelector

//case class Folder(name: Text, Placemarks: Seq[Placemark])

case class Tessellate($: String)

case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates]) extends Geometry

case class Coordinates(coordinates: Seq[Coordinate])

object Coordinates {
  def parse(w: String): Coordinates = {
    val source = Source.fromString(w)
    Coordinates((for (line <- source.getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
  }
}

case class Coordinate(lat: String, long: String, alt: String)

object Coordinate {
  private val latLong: Regex = """\s*([\d\-\.]+),([\d\-\.]+),([\d\-\.]+)""".r

  def apply(w: String): Coordinate = w match {
    case latLong(long, lat, alt) => Coordinate(lat, long, alt)
    case _ => throw XmlException(s"bad coordinate string: $w")
  }
}

object KmlExtractors extends Extractors {

  Extractor.translations += "coordinates" -> "coordinates"

  import Extractors._

  implicit val extractorKmlData: Extractor[KmlData] = extractor10(KmlData.apply)
  implicit val extractorKPP2GeometryData: Extractor[KmlData => GeometryData] = extractorPartial0[KmlData, GeometryData](GeometryData.applyFunction)
  implicit val extractorGeometryData: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorKPP2GeometryData)
  implicit val extractorPair: Extractor[Pair] = extractor20(Pair)
  implicit val extractorMultiPair: MultiExtractor[Seq[Pair]] = multiExtractor[Pair]
  implicit val extractorCoordinates: Extractor[Coordinates] = (node: Node) => Success(Coordinates.parse(node.text))
  implicit val extractorMultiCoordinates: MultiExtractor[Seq[Coordinates]] = multiExtractor[Coordinates]
  implicit val extractMaybeOpen: Extractor[Option[Int]] = extractorOption
  implicit val extractorStyleSelector: Extractor[StyleSelector] = extractorAlt[StyleSelector, Style, StyleMap]
  implicit val extractorMultiStyleSelector: MultiExtractor[Seq[StyleSelector]] = multiExtractor[StyleSelector]
  implicit val extractorKD2FD: Extractor[KmlData => FeatureData] = extractorPartial41(FeatureData.apply)
  implicit val extractorTessellate: Extractor[Tessellate] = extractor10(Tessellate)
  implicit val extractorLineString: Extractor[LineString] = extractor11(LineString)
  implicit val extractorGD2Point: Extractor[GeometryData => Point] = extractorPartial01(Point.apply)
  implicit val extractorPoint: Extractor[Point] = extractorPartial[GeometryData, Point](extractorGD2Point)
  implicit val extractorGeometry: Extractor[Geometry] = none[Geometry].orElse[Point]().orElse[LineString]()
  implicit val extractorMultiGeometry: MultiExtractor[Seq[Geometry]] = multiExtractor[Geometry]
  implicit val extractorFeatureData: Extractor[FeatureData] = extractorPartial[KmlData, FeatureData](extractorKD2FD)
  implicit val extractorFD2Placemark: Extractor[FeatureData => Placemark] = extractorPartial01(Placemark.apply)
  implicit val extractorPlacemark: Extractor[Placemark] = extractorPartial[FeatureData, Placemark](extractorFD2Placemark)
  implicit val extractorMultiPoint: MultiExtractor[Seq[Point]] = multiExtractor[Point]
  implicit val extractorFill: Extractor[Fill] = extractor10(Fill)
  implicit val extractorOutline: Extractor[Outline] = extractor10(Outline)
  implicit val extractorKD2Scale: Extractor[KmlData => Scale] = extractorPartial10(Scale.apply)
  implicit val extractorScale: Extractor[Scale] = extractorPartial[KmlData, Scale](extractorKD2Scale)
  implicit val extractorHeading: Extractor[Heading] = extractor10(Heading)
  implicit val extractMaybeHeading: Extractor[Option[Heading]] = extractorOption
  implicit val extractorListItemType: Extractor[ListItemType] = extractor10(ListItemType)
  implicit val extractMaybeListItemType: Extractor[Option[ListItemType]] = extractorOption
  implicit val extractorState: Extractor[State] = extractor10(State)
  implicit val extractorTextColor: Extractor[TextColor] = extractor10(TextColor)
  implicit val extractMaybeTextColor: Extractor[Option[TextColor]] = extractorOption
  implicit val extractorItemIcon: Extractor[ItemIcon] = extractor20(ItemIcon)
  implicit val extractMaybeItemIcon: Extractor[Option[ItemIcon]] = extractorOption
  implicit val extractorIcon: Extractor[Icon] = extractor10(Icon)
  implicit val extractorBgColor: Extractor[BgColor] = extractor10(BgColor)
  implicit val extractorMaybeBgColor: Extractor[Option[BgColor]] = extractorOption
  implicit val extractorColor: Extractor[Color] = extractor10(Color)
  implicit val extractorWidth: Extractor[Width] = extractor10(Width)
  implicit val extractorHotspot: Extractor[HotSpot] = extractor40(HotSpot)
  implicit val extractorDisplayMode: Extractor[DisplayMode] = extractor10(DisplayMode)
  implicit val extractorColorMode: Extractor[ColorMode] = extractor10(ColorMode)
  implicit val extractMaybeColorMode: Extractor[Option[ColorMode]] = extractorOption
  implicit val extractorKPP2SubStyleData: Extractor[KmlData => SubStyleData] = extractorPartial0[KmlData, SubStyleData](SubStyleData.applyFunction)
  implicit val extractorSubStyleData: Extractor[SubStyleData] = extractorPartial[KmlData, SubStyleData](extractorKPP2SubStyleData)
  implicit val extractorSSP2ColorStyleData: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(ColorStyleData.apply)
  implicit val extractorColorStyleData: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorSSP2ColorStyleData)
  implicit val extractorCSP2PolyStyle: Extractor[ColorStyleData => PolyStyle] = extractorPartial20(PolyStyle.apply)
  implicit val extractorPolyStyle: Extractor[PolyStyle] = extractorPartial[ColorStyleData, PolyStyle](extractorCSP2PolyStyle)
  implicit val extractorCSP2ListStyle: Extractor[ColorStyleData => ListStyle] = extractorPartial30(ListStyle.apply)
  implicit val extractorListStyle: Extractor[ListStyle] = extractorPartial[ColorStyleData, ListStyle](extractorCSP2ListStyle)
  implicit val extractorCSP2IconStyle: Extractor[ColorStyleData => IconStyle] = extractorPartial40(IconStyle.apply)
  implicit val extractorIconStyle: Extractor[IconStyle] = extractorPartial[ColorStyleData, IconStyle](extractorCSP2IconStyle)
  implicit val extractorCSP2BalloonStyle: Extractor[ColorStyleData => BalloonStyle] = extractorPartial40(BalloonStyle.apply)
  implicit val extractorBalloonStyle: Extractor[BalloonStyle] = extractorPartial[ColorStyleData, BalloonStyle](extractorCSP2BalloonStyle)
  implicit val extractorCSP2LabelStyle: Extractor[ColorStyleData => LabelStyle] = extractorPartial10(LabelStyle.apply)
  implicit val extractorLabelStyle: Extractor[LabelStyle] = extractorPartial[ColorStyleData, LabelStyle](extractorCSP2LabelStyle)
  implicit val extractorCSP2LineStyle: Extractor[ColorStyleData => LineStyle] = extractorPartial10(LineStyle.apply)
  implicit val extractorLineStyle: Extractor[LineStyle] = extractorPartial[ColorStyleData, LineStyle](extractorCSP2LineStyle)
  implicit val extractMaybeIconStyle: Extractor[Option[IconStyle]] = extractorOption
  implicit val extractMaybeLabelStyle: Extractor[Option[LabelStyle]] = extractorOption
  implicit val extractMaybeBalloonStyle: Extractor[Option[BalloonStyle]] = extractorOption
  implicit val extractMaybeLineStyle: Extractor[Option[LineStyle]] = extractorOption
  implicit val extractorColorStyle: Extractor[ColorStyle] = Extractor.none[ColorStyle].orElse[BalloonStyle]().orElse[LineStyle]().orElse[IconStyle]().orElse[ListStyle]().orElse[PolyStyle]().orElse[LabelStyle]()
  implicit val extractorMultiColorStyle: MultiExtractor[Seq[ColorStyle]] = multiExtractor[ColorStyle]
  implicit val extractorKPP2StyleSelectorData: Extractor[KmlData => StyleSelectorData] = extractorPartial0[KmlData, StyleSelectorData](StyleSelectorData.applyFunction)
  implicit val extractorStyleSelectorData: Extractor[StyleSelectorData] = extractorPartial[KmlData, StyleSelectorData](extractorKPP2StyleSelectorData)
  implicit val extractorBT1: Extractor[StyleSelectorData => Style] = extractorPartial01(Style.apply)
  implicit val extractorBT2: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(StyleMap.apply)
  implicit val extractorMultiLineString: MultiExtractor[Seq[LineString]] = multiExtractor[LineString]
  implicit val extractorMultiPlacemark: MultiExtractor[Seq[Placemark]] = multiExtractor[Placemark]
  implicit val extractorFD2ContainerData: Extractor[FeatureData => ContainerData] = extractorPartial0[FeatureData, ContainerData](ContainerData.applyFunction)
  implicit val extractorContainerData: Extractor[ContainerData] = extractorPartial[FeatureData, ContainerData](extractorFD2ContainerData)
  implicit val extractorMultiFeature: MultiExtractor[Seq[Feature]] = multiExtractor[Feature]
  implicit val extractorCD2Folder: Extractor[ContainerData => Folder] = extractorPartial01(Folder.apply)
  implicit val extractorContainer: Extractor[Container] = extractor0(Container.applyFunction)
  implicit val extractorFolder: Extractor[Folder] = extractorPartial(extractorCD2Folder)
  implicit val extractorMultiStyleMap: MultiExtractor[Seq[StyleMap]] = multiExtractor[StyleMap]
  implicit val extractorMultiFolder: MultiExtractor[Seq[Folder]] = multiExtractor[Folder]
  implicit val extractorCD2Document: Extractor[ContainerData => Document] = extractorPartial01(Document.apply)
  implicit val extractorDocument: Extractor[Document] = extractorPartial(extractorCD2Document)
  implicit val extractorMultiDocument: MultiExtractor[Seq[Document]] = multiExtractor[Document]
  implicit val extractorKml: Extractor[KML] = extractor01(KML)
  implicit val extractorMultiKml: MultiExtractor[Seq[KML]] = multiExtractor[KML]

  implicit def extractorFeature: Extractor[Feature] = none[Feature].orElse[Container]()


  implicit def extractorStyle: Extractor[Style] = extractorPartial[StyleSelectorData, Style](extractorBT1)

  implicit def extractorStyleMap: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorBT2)
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

  implicit val rendererOptionString: Renderable[Option[String]] = optionRenderer
  implicit val rendererKmlData: Renderable[KmlData] = renderer1(KmlData.apply)
  implicit val rendererGeometryData: Renderable[GeometryData] = renderer0Super(GeometryData.apply)(_.kmlData)
  implicit val rendererFeatureData: Renderable[FeatureData] = renderer5Super(FeatureData.apply)(_.kmlData)
  implicit val rendererScale: Renderable[Scale] = renderer1Super(Scale.apply)(_.kmlData)
  implicit val rendererIcon: Renderable[Icon] = renderer1(Icon)
  implicit val rendererColor: Renderable[Color] = renderer1(Color)
  implicit val rendererBgColor: Renderable[BgColor] = renderer1(BgColor)
  implicit val rendererOptionBgColor: Renderable[Option[BgColor]] = optionRenderer
  implicit val rendererTextColor: Renderable[TextColor] = renderer1(TextColor)
  implicit val rendererOptionTextColor: Renderable[Option[TextColor]] = optionRenderer
  implicit val rendererColorMode: Renderable[ColorMode] = renderer1(ColorMode)
  implicit val rendererOptionColorMode: Renderable[Option[ColorMode]] = optionRenderer
  implicit val rendererWidth: Renderable[Width] = renderer1(Width)
  implicit val rendererHeading: Renderable[Heading] = renderer1(Heading)
  implicit val rendererFill: Renderable[Fill] = renderer1(Fill)
  implicit val rendererOutline: Renderable[Outline] = renderer1(Outline)
  implicit val rendererOptionHeading: Renderable[Option[Heading]] = optionRenderer
  implicit val rendererListItemType: Renderable[ListItemType] = renderer1(ListItemType)
  implicit val rendererOptionListItemType: Renderable[Option[ListItemType]] = optionRenderer
  implicit val rendererHotSpot: Renderable[HotSpot] = renderer4(HotSpot)
  implicit val rendererState: Renderable[State] = renderer1(State)
  implicit val rendererOptionState: Renderable[Option[State]] = optionRenderer
  implicit val rendererItemIcon: Renderable[ItemIcon] = renderer2(ItemIcon)
  implicit val rendererOptionItemIcon: Renderable[Option[ItemIcon]] = optionRenderer
  private val fKP2SSP: KmlData => SubStyleData = k => new SubStyleData(k)
  implicit val rendererSubStyleData: Renderable[SubStyleData] = renderer0Super(fKP2SSP)(x => x.kmlData)
  implicit val rendererColorStyleData: Renderable[ColorStyleData] = renderer2Super(ColorStyleData.apply)(x => x.subStyleData)
  implicit val rendererIconStyle: Renderable[IconStyle] = renderer4Super(IconStyle.apply)(x => x.colorStyleData)
  implicit val rendererDisplayMode: Renderable[DisplayMode] = renderer1(DisplayMode)
  implicit val rendererBalloonStyle: Renderable[BalloonStyle] = renderer4Super(BalloonStyle.apply)(_.colorStyleData)
  implicit val rendererLabelStyle: Renderable[LabelStyle] = renderer1Super(LabelStyle.apply)(_.colorStyleData)
  implicit val rendererLineStyle: Renderable[LineStyle] = renderer1Super(LineStyle.apply)(_.colorStyleData)
  implicit val rendererListStyle: Renderable[ListStyle] = renderer3Super(ListStyle.apply)(_.colorStyleData)
  implicit val rendererPolyStyle: Renderable[PolyStyle] = renderer2Super(PolyStyle.apply)(_.colorStyleData)
  implicit val rendererOptionLineStyle: Renderable[Option[LineStyle]] = optionRenderer
  implicit val rendererOptionLabelStyle: Renderable[Option[LabelStyle]] = optionRenderer
  implicit val rendererOptionBalloonStyle: Renderable[Option[BalloonStyle]] = optionRenderer
  implicit val rendererOptionIconStyle: Renderable[Option[IconStyle]] = optionRenderer
  implicit val rendererStyleSelectorData: Renderable[StyleSelectorData] = renderer1(StyleSelectorData.apply)
  implicit val rendererColorStyle: Renderable[ColorStyle] = rendererSuper6[ColorStyle, IconStyle, ListStyle, BalloonStyle, LabelStyle, LineStyle, PolyStyle]
  implicit val rendererColorStyles: Renderable[Seq[ColorStyle]] = sequenceRenderer[ColorStyle]
  implicit val rendererStyle: Renderable[Style] = renderer1Super(Style.apply)(_.styleSelectorData)
  implicit val rendererPair: Renderable[Pair] = renderer2(Pair)
  implicit val rendererSequencePair: Renderable[Seq[Pair]] = sequenceRenderer[Pair]
  implicit val rendererStyleMap: Renderable[StyleMap] = renderer1Super(StyleMap.apply)(_.styleSelectorData)
  implicit val rendererCoordinate: Renderable[Coordinate] = (t: Coordinate, _: Format, _: StateR) => s"${t.long}, ${t.lat}, ${t.alt}"
  implicit val rendererCoordinates1: Renderable[Seq[Coordinate]] = sequenceRendererFormatted[Coordinate](FormatCoordinate)
  implicit val rendererCoordinates: Renderable[Coordinates] = renderer1(Coordinates.apply)
  // TODO refactor the sequenceRendererFormatted method so that its parameter is a Format=>Format function.
  implicit val rendererCoordinates_s: Renderable[Seq[Coordinates]] = sequenceRendererFormatted[Coordinates](FormatXML)
  implicit val rendererTessellate: Renderable[Tessellate] = renderer1(Tessellate)
  implicit val rendererLineString: Renderable[LineString] = renderer2(LineString)
  implicit val rendererLineStrings: Renderable[Seq[LineString]] = sequenceRenderer[LineString]
  implicit val rendererPoint: Renderable[Point] = renderer1Super(Point.apply)(_.geometryData)
  implicit val rendererPoints: Renderable[Seq[Point]] = sequenceRenderer[Point]
  implicit val rendererGeometry: Renderable[Geometry] = rendererSuper2[Geometry, Point, LineString]
  implicit val rendererGeometrys: Renderable[Seq[Geometry]] = sequenceRenderer[Geometry]
  implicit val rendererPlacemark: Renderable[Placemark] = renderer1Super(Placemark.apply)(_.featureData)
  implicit val rendererPlacemarks: Renderable[Seq[Placemark]] = sequenceRenderer[Placemark]
  implicit val rendererContainerData: Renderable[ContainerData] = renderer0Super(ContainerData.applyFunction)(_.featureData)
  implicit val rendererContainer: Renderable[Container] = rendererSuper2[Container, Folder, Document]
  implicit val rendererFeature: Renderable[Feature] = rendererSuper1[Feature, Container]
  implicit val rendererFeatures: Renderable[Seq[Feature]] = sequenceRenderer[Feature]
  implicit val rendererFolder: Renderable[Folder] = renderer1Super(Folder.apply)(_.containerData)
  implicit val rendererFolders: Renderable[Seq[Folder]] = sequenceRenderer[Folder]
  implicit val rendererDocument: Renderable[Document] = renderer1Super(Document.apply)(_.containerData)
  implicit val rendererDocuments: Renderable[Seq[Document]] = sequenceRenderer[Document]
  implicit val rendererStyles: Renderable[Seq[Style]] = sequenceRenderer[Style]
  implicit val rendererStyleMaps: Renderable[Seq[StyleMap]] = sequenceRenderer[StyleMap]
  implicit val rendererStyleType: Renderable[StyleSelector] = rendererSuper2[StyleSelector, Style, StyleMap]
  implicit val rendererStyleTypes: Renderable[Seq[StyleSelector]] = sequenceRenderer[StyleSelector]
  implicit val renderOptionOpen: Renderable[Option[Int]] = optionRenderer
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

  private def loadKML(file: String): KML = {
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
