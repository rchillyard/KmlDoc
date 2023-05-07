package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.core.FP.tryNotNull
import com.phasmidsoftware.core.{Text, TryUsing, XmlException}
import com.phasmidsoftware.kmldoc.KmlRenderers.sequenceRendererFormatted
import com.phasmidsoftware.render._
import com.phasmidsoftware.xml.Extractor.stringExtractor
import com.phasmidsoftware.xml.{Extractors, _}
import java.net.URL
import org.slf4j.{Logger, LoggerFactory}
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, NamespaceBinding, XML}

/**
 * Trait KmlObject: abstract super-element of all KML elements. Known in the reference document as Object.
 * See [[https://developers.google.com/kml/documentation/kmlreference KML Reference]]
 *
 * NOTE: abstract elements cannot be represented in Kml documents directly but are always super-elements (and therefore ordinary classes).
 * Their usage is in the properties of other elements where more than one possible element type is allowed.
 *
 * Actual Kml elements (the leaf classes) are all represented here as case classes, which cannot be extended.
 *
 * The arrangement of the traits and classes within this module follows the KML Reference starting with Object and then doing breadth-first top to bottom.
 */
trait KmlObject

/**
 * Properties of KMLObject
 *
 * @param __id an optional identifier.
 */
case class KmlData(__id: Option[String])

/**
 * Companion object to KmlData.
 */
object KmlData extends Extractors with Renderers {
    def nemo: KmlData = KmlData(None)

    import Extractors._
    import Renderers._

    implicit val extractor: Extractor[KmlData] = extractor10(apply) ^^ "extractorKmlData"
    implicit val renderer: Renderable[KmlData] = renderer1(apply) ^^ "rendererKmlData"

    ////  implicit object loggableKmlData extends LoggableAny[KmlData]
}

/**
 * Trait Feature: abstract sub-element of KmlObject.
 * Feature is a sub-type of Object and a super-type of Placemark, Container.
 * See [[https://developers.google.com/kml/documentation/kmlreference#feature Feature]].
 *
 * TODO add Overlay, NetworkLink.
 */
trait Feature extends KmlObject

/**
 * Companion object to Feature.
 */
object Feature extends Extractors with Renderers {
    private val labels: Seq[String] = Seq("Folder", "Document", "Placemark")
    implicit val multiExtractor: MultiExtractor[Seq[Feature]] =
        lazyMultiExtractor(multiExtractor3[Feature, (Folder, Document, Placemark), Folder, Document, Placemark]((f, d, p) => (f, d, p), labels) ^^ "multiExtractorFeature")
    implicit val seqExtractor: Extractor[Seq[Feature]] = seqExtractorByLabel("features", labels)
    implicit val renderer: Renderable[Feature] = new Renderers {}.lazyRenderer(rendererSuper2[Feature, Placemark, Container] ^^ "rendererFeature")
    implicit val seqRenderer: Renderable[Seq[Feature]] = sequenceRenderer[Feature] ^^ "rendererFeatures"
}

/**
 * Properties of a Feature (and therefore all its sub-types).
 *
 * @param name             the name (a Text).
 * @param maybeDescription an optional description: Option[Text].
 * @param maybeStyleUrl    an optional style URL: Option[String].
 * @param maybeOpen        an optional openness designation: Option[Int].
 * @param StyleSelectors   a sequence of StyleSelectors: Seq[StyleSelector].
 * @param kmlData          (auxiliary) member: KmlData.
 */
case class FeatureData(name: Text, maybeDescription: Option[Text], maybeStyleUrl: Option[Text], maybeOpen: Option[Int], StyleSelectors: Seq[StyleSelector])(val kmlData: KmlData)

/**
 * Companion object to Feature.
 */
object FeatureData extends Extractors with Renderers {

    import Extractors._
    import Renderers._

    lazy val extractorPartial: Extractor[KmlData => FeatureData] = extractorPartial41(apply)
    implicit val extractor: Extractor[FeatureData] = extractorPartial[KmlData, FeatureData](extractorPartial) ^^ "extractorFeatureData"
    implicit val renderer: Renderable[FeatureData] = renderer5Super(apply)(_.kmlData) ^^ "rendererFeatureData"
}

/**
 * Trait Geometry: abstract sub-element of KmlObject.
 * Geometry is the super-type of LineString and Point.
 * See [[https://developers.google.com/kml/documentation/kmlreference#geometry Geometry]]
 */
trait Geometry extends KmlObject

/**
 * Companion object to Geometry.
 */
object Geometry extends Extractors with Renderers {
    implicit val multiExtractor: MultiExtractor[Seq[Geometry]] =
        multiExtractor2[Geometry, (LineString, Point), LineString, Point]((l, p) => (l, p), Seq("LineString", "Point")) ^^ "multiExtractorGeometry"
    implicit val renderer: Renderable[Geometry] = rendererSuper2[Geometry, Point, LineString] ^^ "rendererGeometry"
    implicit val seqRenderer: Renderable[Seq[Geometry]] = sequenceRenderer[Geometry] ^^ "rendererGeometrys"
}

/**
 * Properties of Geometry.
 * There are no properties specific to Geometry.
 * See [[https://developers.google.com/kml/documentation/kmlreference#geometry Geometry]]
 *
 * @param kmlData source of properties.
 */
case class GeometryData(kmlData: KmlData)

object GeometryData extends Extractors with Renderers {
    val applyFunction: KmlData => GeometryData = new GeometryData(_)
    val extractorPartial: Extractor[KmlData => GeometryData] = extractorPartial0[KmlData, GeometryData](applyFunction)
    implicit val extractor: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorPartial)
    implicit val renderer: Renderable[GeometryData] = renderer0Super(apply)(_.kmlData) ^^ "rendererGeometryData"
}

/**
 * Case class Icon.
 * NOTE: we do not support Link.
 * See [[https://developers.google.com/kml/documentation/kmlreference#icon Icon]]
 *
 * @param href a URL reference to an image.
 */
case class Icon(href: Text)

object Icon extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Icon] = extractor10(apply) ^^ "extractorIcon"
    implicit val renderer: Renderable[Icon] = renderer1(apply) ^^ "rendererIcon"
}

/**
 * Scale element: sub-element of Object in the Kml reference.
 * Case class to represent a Scale which is represented in XML as, for example: <scale>1.1</scale>
 * See [[https://developers.google.com/kml/documentation/kmlreference#scale Scale]]
 *
 * @param $ the value of the scale (a Double).
 */
case class Scale($: Double)(val kmlData: KmlData) extends KmlObject

/**
 * Companion object to Scale.
 */
object Scale extends Extractors with Renderers {

    import Renderers._

    val partialExtractor: Extractor[KmlData => Scale] = extractorPartial10(apply) ^^ "extractorKD2Scale"
    implicit val extractor: Extractor[Scale] = extractorPartial[KmlData, Scale](partialExtractor)
    implicit val renderer: Renderable[Scale] = renderer1Super(apply)(_.kmlData) ^^ "rendererScale"

    def nemo(x: Double): Scale = new Scale(x)(KmlData.nemo)
}

/**
 * Trait StyleSelector is a sub-trait of KmlObject.
 * It is extended by Style and StyleMap.
 * See [[https://developers.google.com/kml/documentation/kmlreference#styleselector StyleSelector]]
 */
trait StyleSelector extends KmlObject

/**
 * Companion object of StyleSelector.
 */
object StyleSelector extends Extractors with Renderers {
    implicit val extractorMulti: MultiExtractor[Seq[StyleSelector]] =
        multiExtractor2[StyleSelector, (Style, StyleMap), Style, StyleMap]((s, m) => (s, m), Seq("Style", "StyleMap")) ^^ "multiExtractorStyleSelector"
    implicit val renderer: Renderable[StyleSelector] = rendererSuper2[StyleSelector, Style, StyleMap] ^^ "rendererStyleSelector"
    implicit val seqRenderer: Renderable[Seq[StyleSelector]] = sequenceRenderer[StyleSelector] ^^ "rendererStyleSelectors"
}

/**
 * Case class StyleSelectorData: properties of StyleSelector and its sub-classes.
 * There are no properties specific to StyleSelector.
 * See [[https://developers.google.com/kml/documentation/kmlreference#styleselector StyleSelector]]
 *
 * @param kmlData the KmlData reference.
 */
case class StyleSelectorData(kmlData: KmlData)

object StyleSelectorData extends Extractors with Renderers {
    lazy val applyFunction: KmlData => StyleSelectorData = new StyleSelectorData(_)
    lazy val extractorPartial: Extractor[KmlData => StyleSelectorData] = extractorPartial0[KmlData, StyleSelectorData](applyFunction) ^^ "extractorKD2StyleSelectorData"
    implicit val extractor: Extractor[StyleSelectorData] = extractorPartial[KmlData, StyleSelectorData](extractorPartial) ^^ "extractorStyleSelectorData"
    implicit val renderer: Renderable[StyleSelectorData] = renderer0Super(applyFunction)(_.kmlData) ^^ "rendererStyleSelectorData"
}

/**
 * Trait SubStyle which, according to the KML reference extends Object and is extended by ColorStyle, BalloonStyle and ListStyle.
 * Reality seems otherwise, however.
 * See [[https://developers.google.com/kml/documentation/kmlreference KML]]
 */
trait SubStyle extends KmlObject

case class SubStyleData(kmlData: KmlData)

object SubStyleData extends Extractors with Renderers {
    lazy val applyFunction: KmlData => SubStyleData = new SubStyleData(_)
    lazy val extractorPartial: Extractor[KmlData => SubStyleData] = extractorPartial0[KmlData, SubStyleData](applyFunction)
    implicit val extractor: Extractor[SubStyleData] = extractorPartial[KmlData, SubStyleData](extractorPartial) ^^ "extractorSubStyleData"
    implicit val renderer: Renderable[SubStyleData] = renderer0Super(apply)(_.kmlData) ^^ "rendererSubStyleData"
}

/**
 * Placemark: sub-type of Feature.
 * See [[https://developers.google.com/kml/documentation/kmlreference#placemark Placemark]].
 *
 * @param Geometry    a sequence of Geometry elements (where Geometry is an abstract super-type).
 * @param featureData the (auxiliary) FeatureData, shared by sub-elements.
 */
case class Placemark(Geometry: Seq[Geometry])(val featureData: FeatureData) extends Feature

/**
 * Companion object to Placemark.
 */
object Placemark extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[FeatureData => Placemark] = extractorPartial01(apply)
    implicit val extractor: Extractor[Placemark] = extractorPartial[FeatureData, Placemark](extractorPartial) ^^ "extractorPlacemark"
    implicit val renderer: Renderable[Placemark] = renderer1Super(apply)(_.featureData) ^^ "rendererPlacemark"
    implicit val seqRenderer: Renderable[Seq[Placemark]] = sequenceRenderer[Placemark] ^^ "rendererPlacemarks"
}

/**
 * Abstract Container element.
 * Container is a sub-type of Feature and a super-type of Folder, Document.
 * See [[https://developers.google.com/kml/documentation/kmlreference#container Container]].
 *
 * A Container has no properties of its own.
 */
trait Container extends Feature

/**
 * Companion object to Container.
 */
object Container extends Extractors with Renderers {
    implicit val multiExtractor: MultiExtractor[Seq[Container]] =
        multiExtractor2[Container, (Folder, Document), Folder, Document]((f, d) => (f, d), Seq("Folder", "Document")) ^^ "multiExtractorContainer"
    implicit val renderer: Renderable[Container] = rendererSuper2[Container, Folder, Document] ^^ "rendererContainer"
}

/**
 * Properties of a Container (and therefore all its sub-types).
 *
 * @param featureData (auxiliary) member: FeatureData, shared by sub-elements.
 */
case class ContainerData(featureData: FeatureData)

/**
 * Companion object to ContainerData.
 */
object ContainerData extends Extractors with Renderers {
    lazy val applyFunction: FeatureData => ContainerData = new ContainerData(_)
    val extractorPartial: Extractor[FeatureData => ContainerData] = extractorPartial0[FeatureData, ContainerData](applyFunction) ^^ "extractorFD2ContainerData"
    implicit val extractor: Extractor[ContainerData] = extractorPartial[FeatureData, ContainerData](extractorPartial) ^^ "extractorContainerData"
    implicit val renderer: Renderable[ContainerData] = renderer0Super(applyFunction)(_.featureData) ^^ "rendererContainerData"
}

/**
 * Case class Point which extends Geometry.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#point Point]]
 *
 * @param coordinates  a sequence of Coordinates objects.
 * @param geometryData the other properties of the Point.
 */
case class Point(coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry

object Point extends Extractors with Renderers {
    val extractorPartial: Extractor[GeometryData => Point] = extractorPartial01(apply)
    implicit val extractor: Extractor[Point] = extractorPartial[GeometryData, Point](extractorPartial) ^^ "extractorPoint"
    implicit val extractorMulti: MultiExtractor[Seq[Point]] = multiExtractorBase[Point]
    implicit val renderer: Renderable[Point] = renderer1Super(apply)(_.geometryData) ^^ "rendererPoint"
    implicit val rendererPoints: Renderable[Seq[Point]] = sequenceRenderer[Point] ^^ "rendererPoints"
}

/**
 * Case class LineString which extends Geometry.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#linestring LineString]]
 *
 * CONSIDER having second parameter set with geometryData (like Point).
 *
 * @param tessellate  the tessellation.
 * @param coordinates a sequence of Coordinates objects.
 */
case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates]) extends Geometry

object LineString extends Extractors with Renderers {
    implicit val extractor: Extractor[LineString] = extractor11(apply) ^^ "extractorLineString"
    implicit val renderer: Renderable[LineString] = renderer2(apply) ^^ "rendererLineString"
    implicit val seqRenderer: Renderable[Seq[LineString]] = sequenceRenderer[LineString] ^^ "rendererLineStrings"
}

/**
 * Case class Style which extends StyleSelector.
 * See [[https://developers.google.com/kml/documentation/kmlreference#style Style]]
 *
 * It seems there are two completely different types of Style element, but they are not distinguished.
 * Type A has IconStyle, LabelStyle, BalloonStyle;
 * Type B has LineStyle.
 *
 * @param Styles a set of different types of Style.
 *               CONSIDER constraining this set to be distinct.
 */
case class Style(Styles: Seq[ColorStyle])(val styleSelectorData: StyleSelectorData) extends StyleSelector

object Style extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[StyleSelectorData => Style] = extractorPartial01(apply) ^^ "extractorSSD2Style"
    implicit val extractor: Extractor[Style] = extractorPartial[StyleSelectorData, Style](extractorPartial) ^^ "extractorStyle"
    implicit val renderer: Renderable[Style] = renderer1Super(apply)(_.styleSelectorData) ^^ "rendererStyle"
    implicit val seqRenderer: Renderable[Seq[Style]] = sequenceRenderer[Style] ^^ "rendererStyles"
}


/**
 * Case class StyleMap which extends StyleSelector.
 * See [[https://developers.google.com/kml/documentation/kmlreference#stylemap StyleMap]]
 * *
 *
 * @param Pairs a sequence of Pair objects.
 */
case class StyleMap(Pairs: Seq[Pair])(val styleSelectorData: StyleSelectorData) extends StyleSelector

object StyleMap extends Extractors {

    import KmlRenderers._

    lazy val extractorPartial: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(apply) ^^ "extractorSSD2StyleMap"
    implicit val extractor: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorPartial) ^^ "extractorStyleMap"
    implicit val multiExtractor: MultiExtractor[Seq[StyleMap]] = multiExtractorBase[StyleMap] ^^ "multiExtractorStyleMap"
    implicit val renderer: Renderable[StyleMap] = renderer1Super(apply)(_.styleSelectorData) ^^ "rendererStyleMap"
    implicit val seqRenderer: Renderable[Seq[StyleMap]] = sequenceRenderer[StyleMap] ^^ "rendererStyleMaps"
}

/**
 * case class BalloonStyle which extends ColorStyle.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#balloonstyle Balloon Style]]
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

object BalloonStyle extends Extractors with Renderers {

    import Renderers._

    lazy val extractorPartial: Extractor[ColorStyleData => BalloonStyle] = extractorPartial40(apply) ^^ "extractorCSD2BalloonStyle"
    implicit val extractor: Extractor[BalloonStyle] = extractorPartial[ColorStyleData, BalloonStyle](extractorPartial) ^^ "extractorBalloonStyle"
    implicit val renderer: Renderable[BalloonStyle] = renderer4Super(apply)(_.colorStyleData) ^^ "rendererBalloonStyle"
    implicit val optRenderer: Renderable[Option[BalloonStyle]] = optionRenderer[BalloonStyle] ^^ "rendererOptionBalloonStyle"
}

/**
 * Case class ListStyle which extends ColorStyle.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#liststyle List Style]]
 *
 * NOTE According to the current KML reference, this object extends SubStyle or ColorStyle (it's not clear which).
 *
 * @param bgColor           optional background color (maybe it isn't optional if there's no color element).
 * @param maybeListItemType optional ListItemType.
 * @param maybeItemIcon     the display mode.
 * @param colorStyleData    the (auxiliary) color style properties.
 */
case class ListStyle(bgColor: BgColor, maybeListItemType: Option[ListItemType], maybeItemIcon: Option[ItemIcon])(val colorStyleData: ColorStyleData) extends ColorStyle

object ListStyle extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[ColorStyleData => ListStyle] = extractorPartial30(apply) ^^ "extractorCSD2ListStyle"
    implicit val extractor: Extractor[ListStyle] = extractorPartial[ColorStyleData, ListStyle](extractorPartial) ^^ "extractorListStyle"
    implicit val renderer: Renderable[ListStyle] = renderer3Super(apply)(_.colorStyleData) ^^ "rendererListStyle"
}

/**
 * Class to define ColorStyle which extends SubStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#colorstyle ColorStyle]]
 *
 * TODO add ColorStyleData
 *
 */
trait ColorStyle extends SubStyle

/**
 * Companion object to ColorStyle.
 */
object ColorStyle extends Extractors with Renderers {
    implicit val multiExtractor: MultiExtractor[Seq[ColorStyle]] =
        multiExtractor6[ColorStyle, (BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle), BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle](
            (p1, p2, p3, p4, p5, p6) => (p1, p2, p3, p4, p5, p6), Seq("BalloonStyle", "ListStyle", "PolyStyle", "LineStyle", "IconStyle", "LabelStyle")
        ) ^^ "multiExtractorColorStyle"
    implicit val renderer: Renderable[ColorStyle] = rendererSuper6[ColorStyle, IconStyle, ListStyle, BalloonStyle, LabelStyle, LineStyle, PolyStyle] ^^ "rendererColorStyle"
    implicit val seqRenderer: Renderable[Seq[ColorStyle]] = sequenceRenderer[ColorStyle] ^^ "rendererColorStyles"
}

/**
 * Case class ColorStyleData to represent the data associated with all ColorStyle elements.
 *
 * @param maybeColor     an optional Color.
 * @param maybeColorMode an optional ColorMode.
 * @param subStyleData   the SubStyleData.
 */
case class ColorStyleData(maybeColor: Option[Color], maybeColorMode: Option[ColorMode])(val subStyleData: SubStyleData)

object ColorStyleData extends Extractors with Renderers {

    lazy val extractorPartial: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(apply) ^^ "extractorSSD2ColorStyleData"
    implicit val extractor: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorPartial) ^^ "extractorColorStyleData"
    implicit val renderer: Renderable[ColorStyleData] = renderer2Super(apply)(x => x.subStyleData) ^^ "rendererColorStyleData"
}

/**
 * Case class Folder: sub-element of Container.
 * See [[https://developers.google.com/kml/documentation/kmlreference#container Folder]].
 *
 * @param features      a sequence of Feature elements (where Feature is an abstract super-type).
 * @param containerData the ContainerData (auxiliary property).
 */
case class Folder(features: Seq[Feature])(val containerData: ContainerData) extends Container

/**
 * Companion object to Folder.
 */
object Folder extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[ContainerData => Folder] = extractorPartial01(apply) ^^ "extractorCD2Folder"
    implicit val extractor: Extractor[Folder] = extractorPartial(extractorPartial) ^^ "extractorFolder"
    implicit val renderer: Renderable[Folder] = renderer1Super(apply)(_.containerData) ^^ "rendererFolder"
    implicit val seqRenderer: Renderable[Seq[Folder]] = sequenceRenderer[Folder] ^^ "rendererFolders"
}

/**
 * Case class Document: sub-element of Container.
 * See [[https://developers.google.com/kml/documentation/kmlreference#document Document]].
 *
 * TODO add Schemas to this case class
 *
 * @param features      a sequence of Features.
 * @param containerData ContainerData (auxiliary property).
 */
case class Document(features: Seq[Feature])(val containerData: ContainerData) extends Container

/**
 * Companion object to Document.
 */
object Document extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[ContainerData => Document] = extractorPartial01(apply) ^^ "extractorCD2Document"
    implicit val extractor: Extractor[Document] = extractorPartial(extractorPartial) ^^ "extractorDocument"
    implicit val renderer: Renderable[Document] = renderer1Super(apply)(_.containerData) ^^ "rendererDocument"
    implicit val seqRenderer: Renderable[Seq[Document]] = sequenceRenderer[Document] ^^ "rendererDocuments"
}

/**
 * Case class LineStyle which extends ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#linestyle Line Style]]
 *
 * @param width          the width of the line.
 * @param colorStyleData other properties.
 */
case class LineStyle(width: Double)(val colorStyleData: ColorStyleData) extends ColorStyle

object LineStyle extends Extractors with Renderers {

    import Renderers._

    lazy val extractorPartial: Extractor[ColorStyleData => LineStyle] = extractorPartial10(apply)
    implicit val extractor: Extractor[LineStyle] = extractorPartial[ColorStyleData, LineStyle](extractorPartial) ^^ "extractorLineStyle"
    implicit val renderer: Renderable[LineStyle] = renderer1Super(apply)(_.colorStyleData) ^^ "rendererLineStyle"
    implicit val optRenderer: Renderable[Option[LineStyle]] = optionRenderer[LineStyle] ^^ "rendererOptionLineStyle"
}

/**
 * Case class PolyStyle which extends ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#polystyle Poly Style]]
 *
 * @param fill           the value of fill.
 * @param outline        the value of outline.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class PolyStyle(fill: Fill, outline: Outline)(val colorStyleData: ColorStyleData) extends ColorStyle

object PolyStyle extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[ColorStyleData => PolyStyle] = extractorPartial20(apply) ^^ "extractorCSD2PolyStyle"
    implicit val extractor: Extractor[PolyStyle] = extractorPartial[ColorStyleData, PolyStyle](extractorPartial) ^^ "extractorPolyStyle"
    implicit val renderer: Renderable[PolyStyle] = renderer2Super(apply)(_.colorStyleData) ^^ "rendererPolyStyle"
}

/**
 * Case class to model IconStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#iconstyle IconStyle]]
 *
 * @param scale          the Scale.
 * @param Icon           the Icon.
 * @param hotSpot        the HotSpot
 * @param maybeHeading   an optional Heading.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class IconStyle(scale: Scale, Icon: Icon, hotSpot: HotSpot, maybeHeading: Option[Heading])(val colorStyleData: ColorStyleData) extends ColorStyle

object IconStyle extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[ColorStyleData => IconStyle] = extractorPartial40(apply) ^^ "extractorCSP2IconStyle"
    implicit val extractor: Extractor[IconStyle] = extractorPartial[ColorStyleData, IconStyle](extractorPartial) ^^ "extractorIconStyle"
    implicit val renderer: Renderable[IconStyle] = renderer4Super(apply)(x => x.colorStyleData) ^^ "rendererIconStyle"
    implicit val optRenderer: Renderable[Option[IconStyle]] = optionRenderer[IconStyle] ^^ "rendererOptionIconStyle"
}

/**
 * Case class LabelStyle which extends ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#labelstyle Label Style]]
 *
 * @param scale          the scale.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class LabelStyle(scale: Scale)(val colorStyleData: ColorStyleData) extends ColorStyle

object LabelStyle extends Extractors with Renderers {
    lazy val extractorPartial: Extractor[ColorStyleData => LabelStyle] = extractorPartial10(apply) ^^ "extractorCSD2LabelStyle"
    implicit val extractor: Extractor[LabelStyle] = extractorPartial[ColorStyleData, LabelStyle](extractorPartial) ^^ "extractorLabelStyle"
    implicit val renderer: Renderable[LabelStyle] = renderer1Super(apply)(_.colorStyleData) ^^ "rendererLabelStyle"
    implicit val optRenderer: Renderable[Option[LabelStyle]] = optionRenderer[LabelStyle] ^^ "rendererOptionLabelStyle"
}

//================Classes which do not appear at top level of KML Reference================================================

case class Tessellate($: String)

object Tessellate extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Tessellate] = extractor10(apply) ^^ "extractorTessellate"
    implicit val renderer: Renderable[Tessellate] = renderer1(apply) ^^ "rendererTessellate"
}

case class Coordinates(coordinates: Seq[Coordinate])

object Coordinates extends Extractors with Renderers {
    implicit val extractor: Extractor[Coordinates] = Extractor(node => Success(Coordinates.parse(node.text))) ^^ "extractorCoordinates"
    implicit val extractorMulti: MultiExtractor[Seq[Coordinates]] = multiExtractorBase[Coordinates] ^^ "multiExtractorCoordinates"
    implicit val rendererCoordinates: Renderable[Coordinates] = renderer1(apply) ^^ "rendererCoordinates"
    implicit val seqRenderer: Renderable[Seq[Coordinates]] = sequenceRendererFormatted[Coordinates](FormatXML) ^^ "rendererCoordinates_s"

    def parse(w: String): Coordinates = Coordinates((for (line <- Source.fromString(w).getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
}

case class Coordinate(long: String, lat: String, alt: String)

object Coordinate {

    // CONSIDER using Parser-combinators here.
    private val longLatAlt: Regex = """^\s*(((-)?(\d+(\.\d*)?)),\s*((-)?(\d+(\.\d*)?)),\s*((-)?(\d+(\.\d*)?)))\s*""".r

    def apply(w: String): Coordinate = w match {
        case longLatAlt(_, long, _, _, _, lat, _, _, _, alt, _, _, _) =>
            println(s"$long, $lat, $alt")
            Coordinate(long, lat, alt)
        case _ => throw XmlException(s"bad coordinate string: $w")
    }

    implicit val renderer: Renderable[Coordinate] = Renderable { (t: Coordinate, _: Format, _: StateR) => Success(s"${t.long}, ${t.lat}, ${t.alt}") } ^^ "rendererCoordinate"
    implicit val seqRenderer: Renderable[Seq[Coordinate]] = sequenceRendererFormatted[Coordinate](KmlRenderers.FormatCoordinate) ^^ "rendererCoordinates1"
}

/**
 * Fill.
 * NOTE that this is a boolean that is represented by 0 or 1.
 *
 * @param boolean whether to fill or not.
 */
case class Fill(boolean: Int)

object Fill extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Fill] = extractor10(apply) ^^ "extractorFill"
    implicit val renderer: Renderable[Fill] = renderer1(apply) ^^ "rendererFill"
}

/**
 * Outline.
 * NOTE that this is a boolean that is represented by 0 or 1.
 *
 * @param boolean whether to outline or not.
 */
case class Outline(boolean: Int)

object Outline extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Outline] = extractor10(apply) ^^ "extractorOutline"
    implicit val renderer: Renderable[Outline] = renderer1(apply) ^^ "rendererOutline"
}

/**
 * Case class to represent a Heading which is represented in XML as, for example: <heading>1.1</heading>
 *
 * @param $ the value of the heading (a Double).
 */
case class Heading($: Double)

object Heading extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Heading] = extractor10(apply) ^^ "extractorHeading"
    implicit val optionExtractor: Extractor[Option[Heading]] = extractorOption[Heading] ^^ "extractMaybeHeading"
    implicit val renderer: Renderable[Heading] = renderer1(apply) ^^ "rendererHeading"
    implicit val optRenderer: Renderable[Option[Heading]] = optionRenderer[Heading] ^^ "rendererOptionHeading"
}

case class BgColor($: String)

object BgColor extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[BgColor] = extractor10(apply) ^^ "extractorBgColor"
    implicit val optExtractor: Extractor[Option[BgColor]] = extractorOption[BgColor] ^^ "extractorMaybeBgColor"
    implicit val renderer: Renderable[BgColor] = renderer1(apply) ^^ "rendererBgColor"
    implicit val seqRenderer: Renderable[Option[BgColor]] = optionRenderer[BgColor] ^^ "rendererOptionBgColor"
}

/**
 * TextColor
 * Used by BalloonStyle.
 *
 * @param $ the color.
 */
case class TextColor($: String)

object TextColor extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[TextColor] = extractor10(apply) ^^ "extractorTextColor"
    implicit val optExtractor: Extractor[Option[TextColor]] = extractorOption[TextColor] ^^ "extractMaybeTextColor"
    implicit val renderer: Renderable[TextColor] = renderer1(apply) ^^ "rendererTextColor"
    implicit val seqRenderer: Renderable[Option[TextColor]] = optionRenderer[TextColor] ^^ "rendererOptionTextColor"
}

/**
 * DisplayMode which has values "default" or "hide".
 * Used by BalloonStyle.
 *
 * @param $ the mode.
 */
case class DisplayMode($: String)

object DisplayMode extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[DisplayMode] = extractor10(apply) ^^ "extractorDisplayMode"
    implicit val renderer: Renderable[DisplayMode] = renderer1(apply) ^^ "rendererDisplayMode"
}

/**
 * ListItemType
 * TODO this should be an enumerated type with values: check,checkOffOnly,checkHideChildren,radioFolder
 *
 * @param $ the value.
 */
case class ListItemType($: String)

object ListItemType extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[ListItemType] = extractor10(apply) ^^ "extractorListItemType"
    implicit val optExtractor: Extractor[Option[ListItemType]] = extractorOption[ListItemType] ^^ "extractMaybeListItemType"
    implicit val renderer: Renderable[ListItemType] = renderer1(apply) ^^ "rendererListItemType"
    implicit val optRenderer: Renderable[Option[ListItemType]] = optionRenderer[ListItemType] ^^ "rendererOptionListItemType"
}

/**
 * State
 * TODO this should be an enumerated type with values: open, closed, error, fetching0, fetching1, or fetching2
 *
 * @param $ the value.
 */
case class State($: String)

object State extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[State] = extractor10(apply) ^^ "extractorState"
    implicit val renderer: Renderable[State] = renderer1(apply) ^^ "rendererState"
    implicit val optRenderer: Renderable[Option[State]] = optionRenderer[State] ^^ "rendererOptionState"
}

/**
 * ItemIcon
 *
 * @param state the state.
 */
case class ItemIcon(state: State, href: Text)

object ItemIcon extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[ItemIcon] = extractor20(apply) ^^ "extractorItemIcon"
    implicit val optExtractor: Extractor[Option[ItemIcon]] = extractorOption[ItemIcon] ^^ "extractMaybeItemIcon"
    implicit val renderer: Renderable[ItemIcon] = renderer2(apply) ^^ "rendererItemIcon"
    implicit val optRenderer: Renderable[Option[ItemIcon]] = optionRenderer[ItemIcon] ^^ "rendererOptionItemIcon"
}

/**
 * Case class to model a HotSpot.
 *
 * @param _x optional x field.
 * @param _xunits optional xunits field.
 * @param _y optional y field.
 * @param _yunits optional yunits field.
 */
case class HotSpot(_x: Int, _xunits: String, _y: Int, _yunits: String)

object HotSpot extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[HotSpot] = extractor40(apply) ^^ "extractorHotspot"
    implicit val renderer: Renderable[HotSpot] = renderer4(apply) ^^ "rendererHotSpot"
}

/**
 * Case class to define Color.  An element of ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#colorstyle ColorStyle]]
 *
 * @param $ the color as a hexadecimal string.
 */
case class Color($: String)

object Color extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Color] = extractor10(apply) ^^ "extractorColor"
    implicit val optExtractor: Extractor[Option[Color]] = extractorOption[Color] ^^ "extractMaybeColor"
    implicit val renderer: Renderable[Color] = renderer1(apply) ^^ "rendererColor"
    implicit val optRenderer: Renderable[Option[Color]] = optionRenderer[Color] ^^ "rendererOptionColor"
}

/**
 * Case class to model ColorMode.
 * See [[https://developers.google.com/kml/documentation/kmlreference#colormode ColorMode]]
 *
 * @param $ the color mode as string: "normal" or "random."
 */
case class ColorMode($: String)

object ColorMode extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[ColorMode] = extractor10(apply) ^^ "extractorColorMode"
    implicit val optExtractor: Extractor[Option[ColorMode]] = extractorOption[ColorMode] ^^ "extractMaybeColorMode"
    implicit val renderer: Renderable[ColorMode] = renderer1(apply) ^^ "rendererColorMode"
    implicit val optRenderer: Renderable[Option[ColorMode]] = optionRenderer[ColorMode] ^^ "rendererOptionColorMode"

}

case class Width($: Double)

object Width extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Width] = extractor10(apply) ^^ "extractorWidth"
    implicit val renderer: Renderable[Width] = renderer1(apply) ^^ "rendererWidth"
}

case class Pair(key: String, styleUrl: String)

object Pair extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Pair] = extractor20(apply) ^^ "extractorPair"
    implicit val multiExtractor: MultiExtractor[Seq[Pair]] = multiExtractorBase[Pair] ^^ "multiExtractorPair"
    implicit val renderer: Renderable[Pair] = renderer2(apply) ^^ "rendererPair"
    implicit val seqRenderer: Renderable[Seq[Pair]] = sequenceRenderer[Pair] ^^ "rendererSequencePair"
}

/**
 * Case class to define a KML object.
 *
 * NOTE WELL: do not be tempted to add "_xmlns" as a member.
 * If you do, you will run into the undocumented(?) "feature" of the Xml library that "xmlns" is a reserved attribute name.
 *
 * @param features a sequence of Feature (typically Document).
 */
case class KML(features: Seq[Feature])

object KML extends Extractors with Renderers {
    implicit val extractor: Extractor[KML] = extractor01(apply) ^^ "extractorKml"
    implicit val renderer: Renderable[KML] = renderer1(apply) ^^ "rendererKml"
    implicit val multiExtractor: MultiExtractor[Seq[KML]] = multiExtractorBase[KML] ^^ "multiExtractorKml"
}

case class KML_Binding(kml: KML, binding: NamespaceBinding)

object KML_Binding {
    implicit val extractor: Extractor[KML_Binding] = Extractor {
        node =>
            implicitly[Extractor[KML]].extract(node) map (KML_Binding(_, node.scope))
    }
    implicit val renderer: Renderable[KML_Binding] = Renderable {
        (t: KML_Binding, format: Format, stateR: StateR) =>
            TryUsing(stateR.addAttribute(s"""${t.binding}"""))(rs => implicitly[Renderable[KML]].render(t.kml, format, rs))
    } ^^ "rendererKml_Binding"
}

object KmlRenderers extends Renderers {

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

    // TODO refactor the sequenceRendererFormatted method so that its parameter is a Format=>Format function.
}

// CONSIDER Rename as KML
object KMLCompanion {

    val logger: Logger = LoggerFactory.getLogger(KML.getClass)

    // TESTME
    def loadKML(resource: URL): Try[Seq[Feature]] = {
        val z: Try[URL] = tryNotNull(resource)(s"resource $resource is null")
        z flatMap (x => loadKML(x.getPath))
    }

    private def loadKML(file: String): Try[Seq[Feature]] = {
        require(file != null)
        val xml: Elem = XML.loadFile(file)

        val kys: Seq[Try[Seq[Feature]]] = for (kml <- xml \\ "kml") yield Extractor.extractAll[Seq[Feature]](kml)
        kys.headOption match {
            case Some(ky) => ky
            case _ => Failure(new NoSuchElementException)
        }
    }
}

object Test extends App {
    // TESTME -- it appears not to work.
    val kml: Try[Seq[Feature]] = KMLCompanion.loadKML(KML.getClass.getResource("sample.kml"))
    kml foreach (fs => fs foreach (f => println(s"KML: $f")))
}
