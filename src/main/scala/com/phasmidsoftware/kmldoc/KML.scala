package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.core.FP.tryNotNull
import com.phasmidsoftware.core.{Text, TryUsing, XmlException}
import com.phasmidsoftware.kmldoc.KmlRenderers.sequenceRendererFormatted
import com.phasmidsoftware.render._
import com.phasmidsoftware.xml.Extractor.intExtractor
import com.phasmidsoftware.xml.MultiExtractorBase.{NonNegative, Positive}
import com.phasmidsoftware.xml._
import java.net.URL
import org.slf4j.{Logger, LoggerFactory}
import scala.io.Source
import scala.reflect.ClassTag
import scala.util._
import scala.util.matching.Regex
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
    implicit val renderer: Renderer[KmlData] = renderer1(apply) ^^ "rendererKmlData"
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

    implicit val extractorSeq: MultiExtractor[Seq[Feature]] =
        MultiExtractor.createLazy(multiExtractor3[Feature, (Folder, Document, Placemark), Folder, Document, Placemark]((f, d, p) => (f, d, p), Seq("Folder", "Document", "Placemark")) ^^ "multiExtractorFeature")
    implicit val renderer: Renderer[Feature] = Renderer.createLazy(rendererSuper2[Feature, Placemark, Container] ^^ "rendererFeature")
    implicit val rendererSeq: Renderer[Seq[Feature]] = sequenceRenderer[Feature] ^^ "rendererFeatures"
}

/**
 * Properties of a Feature (and therefore all its sub-types).
 *
 * CONSIDER use StyleURL type for maybeStyleURL.
 *
 * CONSIDER redefining abstractView so that it can have only one optional AbstractView (according to KML spec.)
 *
 * @param name             the name (a Text).
 * @param maybeDescription an optional description: Option[Text].
 * @param maybeStyleUrl    an optional style URL: Option[String].
 * @param maybeOpen        an optional openness designation: Option[Int].
 * @param StyleSelectors   a sequence of StyleSelectors: Seq[StyleSelector].
 * @param kmlData          (auxiliary) member: KmlData.
 */
case class FeatureData(name: Text, maybeDescription: Option[Text], maybeStyleUrl: Option[Text], maybeOpen: Option[Open], maybeVisibility: Option[Visibility], StyleSelectors: Seq[StyleSelector], abstractView: Seq[AbstractView])(val kmlData: KmlData)

/**
 * Companion object to Feature.
 */
object FeatureData extends Extractors with Renderers {

    import Renderers._

    val extractorPartial: Extractor[KmlData => FeatureData] = extractorPartial52(apply)
    implicit val extractor: Extractor[FeatureData] = extractorPartial[KmlData, FeatureData](extractorPartial) ^^ "extractorFeatureData"
    implicit val renderer: Renderer[FeatureData] = renderer7Super(apply)(_.kmlData) ^^ "rendererFeatureData"
}

/**
 * Trait Geometry: abstract sub-element of KmlObject.
 * Geometry is the super-type of LineString, Polygon, LinearRing, and Point.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#geometry Geometry]]
 */
trait Geometry extends KmlObject

/**
 * Companion object to Geometry.
 */
object Geometry extends Extractors with Renderers {
    implicit val extractorSeq: MultiExtractor[Seq[Geometry]] =
        multiExtractor4[Geometry, (LineString, Point, Polygon, LinearRing), LineString, Point, Polygon, LinearRing]((ls, pt, pg, lr) => (ls, pt, pg, lr), Seq("LineString", "Point", "Polygon", "LinearRing")) ^^ "multiExtractorGeometry"
    implicit val renderer: Renderer[Geometry] = rendererSuper4[Geometry, Point, LineString, Polygon, LinearRing] ^^ "rendererGeometry"
    implicit val rendererSeq: Renderer[Seq[Geometry]] = sequenceRenderer[Geometry] ^^ "rendererGeometrys"
}

/**
 * Properties of Geometry.
 * There are no properties specific to Geometry.
 * See [[https://developers.google.com/kml/documentation/kmlreference#geometry Geometry]]
 *
 * @param kmlData source of properties.
 */
case class GeometryData(maybeExtrude: Option[Extrude], maybeAltitudeMode: Option[AltitudeMode])(val kmlData: KmlData)

object GeometryData extends Extractors with Renderers {

    private val extractorPartial: Extractor[KmlData => GeometryData] = extractorPartial20(apply)
    implicit val extractor: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorPartial)
    implicit val renderer: Renderer[GeometryData] = renderer2Super(apply)(_.kmlData) ^^ "rendererGeometryData"
}

/**
 * Trait AbstractView: abstract sub-element of KmlObject.
 * AbstractView is the super-type of Camera and LookAt.
 * See [[https://developers.google.com/kml/documentation/kmlreference#abstractview AbstractView]]
 */
trait AbstractView extends KmlObject

/**
 * Companion object to AbstractView.
 */
object AbstractView extends Extractors with Renderers {
    implicit val extractorSeq: MultiExtractor[Seq[AbstractView]] =
        multiExtractor2[AbstractView, (Camera, LookAt), Camera, LookAt]((l, p) => (l, p), Seq("Camera", "LookAt")) ^^ "multiExtractorAbstractView"
    implicit val renderer: Renderer[AbstractView] = rendererSuper2[AbstractView, Camera, LookAt] ^^ "rendererAbstractView"
    implicit val rendererSeq: Renderer[Seq[AbstractView]] = sequenceRenderer[AbstractView] ^^ "rendererAbstractViews"
}

/**
 * Properties of AbstractView.
 * There are no properties specific to AbstractView.
 * See [[https://developers.google.com/kml/documentation/kmlreference#abstractview AbstractView]]
 *
 * @param kmlData source of properties.
 */
case class AbstractViewData(kmlData: KmlData)

object AbstractViewData extends Extractors with Renderers {
    private val applyFunction: KmlData => AbstractViewData = new AbstractViewData(_)
    val extractorPartial: Extractor[KmlData => AbstractViewData] = extractorPartial0[KmlData, AbstractViewData](applyFunction)
    implicit val extractor: Extractor[AbstractViewData] = extractorPartial[KmlData, AbstractViewData](extractorPartial)
    implicit val renderer: Renderer[AbstractViewData] = renderer0Super(apply)(_.kmlData) ^^ "rendererAbstractViewData"
}

/**
 * [[https://developers.google.com/kml/documentation/kmlreference#lookat LookAt]]
 */
case class LookAt(longitude: Longitude, latitude: Latitude, maybeAltitude: Option[Altitude], heading: Heading, tilt: Tilt, range: Range, maybeAltitudeMode: Option[AltitudeMode])(val abstractViewData: AbstractViewData) extends AbstractView

object LookAt extends Extractors with Renderers {

    val extractorPartial: Extractor[AbstractViewData => LookAt] = extractorPartial70(apply)
    implicit val extractor: Extractor[LookAt] = extractorPartial[AbstractViewData, LookAt](extractorPartial)
    implicit val extractorOpt: Extractor[Option[LookAt]] = extractorOption[LookAt]
    implicit val renderer: Renderer[LookAt] = renderer7Super(apply)(_.abstractViewData)
    implicit val rendererOpt: Renderer[Option[LookAt]] = optionRenderer[LookAt]
}

/**
 * [[https://developers.google.com/kml/documentation/kmlreference#camera Camera]]
 */
case class Camera(long: Longitude, lat: Latitude, alt: Altitude, heading: Heading, tilt: Tilt, roll: Roll, maybeAltitudeMode: Option[AltitudeMode])(val abstractViewData: AbstractViewData) extends AbstractView

object Camera extends Extractors with Renderers {

    val extractorPartial: Extractor[AbstractViewData => Camera] = extractorPartial70(apply)
    implicit val extractor: Extractor[Camera] = extractorPartial[AbstractViewData, Camera](extractorPartial)
    implicit val renderer: Renderer[Camera] = renderer7Super(apply)(_.abstractViewData)
    implicit val rendererOpt: Renderer[Option[Camera]] = optionRenderer[Camera]
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
    implicit val renderer: Renderer[Icon] = renderer1(apply) ^^ "rendererIcon"
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

    val extractorPartial: Extractor[KmlData => Scale] = extractorPartial10(apply) ^^ "extractorKD2Scale"
    implicit val extractor: Extractor[Scale] = extractorPartial[KmlData, Scale](extractorPartial)
    implicit val extractorOpt: Extractor[Option[Scale]] = extractorOption[Scale] ^^ "extractMaybeScale"
    implicit val renderer: Renderer[Scale] = renderer1Super(apply)(_.kmlData) ^^ "rendererScale"
    implicit val rendererOpt: Renderer[Option[Scale]] = optionRenderer[Scale] ^^ "rendererOptionScale"

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
    implicit val extractorSeq: MultiExtractor[Seq[StyleSelector]] =
        multiExtractor2[StyleSelector, (StyleMap, Style), StyleMap, Style]((s, m) => (s, m), Seq("StyleMap", "Style")) ^^ "multiExtractorStyleSelector"
    implicit val renderer: Renderer[StyleSelector] = rendererSuper2[StyleSelector, Style, StyleMap] ^^ "rendererStyleSelector"
    implicit val rendererSeq: Renderer[Seq[StyleSelector]] = sequenceRenderer[StyleSelector] ^^ "rendererStyleSelectors"
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
    private val applyFunction: KmlData => StyleSelectorData = new StyleSelectorData(_)
    val extractorPartial: Extractor[KmlData => StyleSelectorData] = extractorPartial0[KmlData, StyleSelectorData](applyFunction) ^^ "extractorKD2StyleSelectorData"
    implicit val extractor: Extractor[StyleSelectorData] = extractorPartial[KmlData, StyleSelectorData](extractorPartial) ^^ "extractorStyleSelectorData"
    implicit val renderer: Renderer[StyleSelectorData] = renderer0Super(applyFunction)(_.kmlData) ^^ "rendererStyleSelectorData"
}

/**
 * Trait SubStyle which, according to the KML reference extends Object and is extended by ColorStyle, BalloonStyle and ListStyle.
 * Reality seems otherwise, however.
 * See [[https://developers.google.com/kml/documentation/kmlreference KML]]
 */
trait SubStyle extends KmlObject

/**
 * Companion object to SubStyle.
 */
object SubStyle extends Extractors with Renderers {
    // CONSIDER invoking ColorStyle
    implicit val extractorSeq: MultiExtractor[Seq[SubStyle]] =
        multiExtractor6[SubStyle, (BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle), BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle](
            (p1, p2, p3, p4, p5, p6) => (p1, p2, p3, p4, p5, p6), Seq("BalloonStyle", "ListStyle", "PolyStyle", "LineStyle", "IconStyle", "LabelStyle")
        ) ^^ "multiExtractorSubStyle"
    implicit val renderer: Renderer[SubStyle] = rendererSuper6[SubStyle, IconStyle, ListStyle, BalloonStyle, LabelStyle, LineStyle, PolyStyle] ^^ "rendererSubStyle"
    implicit val rendererSeq: Renderer[Seq[SubStyle]] = sequenceRenderer[SubStyle] ^^ "rendererSubStyles"
}

case class SubStyleData(kmlData: KmlData)

object SubStyleData extends Extractors with Renderers {
    private val applyFunction: KmlData => SubStyleData = new SubStyleData(_)
    val extractorPartial: Extractor[KmlData => SubStyleData] = extractorPartial0[KmlData, SubStyleData](applyFunction)
    implicit val extractor: Extractor[SubStyleData] = extractorPartial[KmlData, SubStyleData](extractorPartial) ^^ "extractorSubStyleData"
    implicit val renderer: Renderer[SubStyleData] = renderer0Super(apply)(_.kmlData) ^^ "rendererSubStyleData"
}

/**
 * Placemark: sub-type of Feature.
 * See [[https://developers.google.com/kml/documentation/kmlreference#placemark Placemark]].
 *
 * CONSIDER restricting the number of Geometry elements to one.
 *
 * @param Geometry    a sequence of Geometry elements (where Geometry is an abstract super-type).
 * @param featureData the (auxiliary) FeatureData, shared by sub-elements.
 */
case class Placemark(Geometry: Seq[Geometry])(val featureData: FeatureData) extends Feature

/**
 * Companion object to Placemark.
 */
object Placemark extends Extractors with Renderers {
    val extractorPartial: Extractor[FeatureData => Placemark] = extractorPartial01(apply)
    implicit val extractor: Extractor[Placemark] = extractorPartial[FeatureData, Placemark](extractorPartial) ^^ "extractorPlacemark"
    implicit val renderer: Renderer[Placemark] = renderer1Super(apply)(_.featureData) ^^ "rendererPlacemark"
    implicit val rendererSeq: Renderer[Seq[Placemark]] = sequenceRenderer[Placemark] ^^ "rendererPlacemarks"
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
    implicit val extractorSeq: MultiExtractor[Seq[Container]] =
        multiExtractor2[Container, (Folder, Document), Folder, Document]((f, d) => (f, d), Seq("Folder", "Document")) ^^ "multiExtractorContainer"
    implicit val renderer: Renderer[Container] = rendererSuper2[Container, Folder, Document] ^^ "rendererContainer"
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
    private val applyFunction: FeatureData => ContainerData = new ContainerData(_)
    val extractorPartial: Extractor[FeatureData => ContainerData] = extractorPartial0[FeatureData, ContainerData](applyFunction) ^^ "extractorFD2ContainerData"
    implicit val extractor: Extractor[ContainerData] = extractorPartial[FeatureData, ContainerData](extractorPartial) ^^ "extractorContainerData"
    implicit val renderer: Renderer[ContainerData] = renderer0Super(applyFunction)(_.featureData) ^^ "rendererContainerData"
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
    private val extractorPartial: Extractor[GeometryData => Point] = extractorPartial01(apply)
    implicit val extractor: Extractor[Point] = extractorPartial[GeometryData, Point](extractorPartial) ^^ "extractorPoint"
    implicit val renderer: Renderer[Point] = renderer1Super(apply)(_.geometryData) ^^ "rendererPoint"
    implicit val rendererSeq: Renderer[Seq[Point]] = sequenceRenderer[Point] ^^ "rendererPoints"
}

/**
 * Case class LineString which extends Geometry.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#linestring LineString]]
 *
 * @param tessellate  the tessellation.
 * @param coordinates a sequence of Coordinates objects.
 */
case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry

object LineString extends Extractors with Renderers {
    private val extractorPartial: Extractor[GeometryData => LineString] = extractorPartial11(apply)
    implicit val extractor: Extractor[LineString] = extractorPartial[GeometryData, LineString](extractorPartial) ^^ "extractorLineString"
    implicit val renderer: Renderer[LineString] = renderer2Super(apply)(_.geometryData) ^^ "rendererLineString"
    implicit val rendererSeq: Renderer[Seq[LineString]] = sequenceRenderer[LineString] ^^ "rendererLineStrings"
}

/**
 * Case class LineString which extends Geometry.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#linestring LineString]]
 *
 * @param maybeTessellate the tessellation.
 * @param outerBoundaryIs an OuterBoundaryIs object.
 * @param innerBoundaryIs a sequence of InnerBoundaryIs objects.
 */
case class Polygon(maybeTessellate: Option[Tessellate], outerBoundaryIs: OuterBoundaryIs, innerBoundaryIs: Seq[InnerBoundaryIs])(val geometryData: GeometryData) extends Geometry

object Polygon extends Extractors with Renderers {
    private val extractorPartial: Extractor[GeometryData => Polygon] = extractorPartial21(apply)
    implicit val extractor: Extractor[Polygon] = extractorPartial[GeometryData, Polygon](extractorPartial) ^^ "extractorPolygon"
    implicit val renderer: Renderer[Polygon] = renderer3Super(apply)(_.geometryData) ^^ "rendererPolygon"
}

/**
 * [[https://developers.google.com/kml/documentation/kmlreference#outerboundaryis outerBoundaryIs]]
 *
 * @param LinearRing the linear ring which makes up this outer boundary.
 */
case class OuterBoundaryIs(LinearRing: LinearRing)

/**
 * Companion class to OuterBoundaryIs.
 */
object OuterBoundaryIs extends Extractors with Renderers {
    implicit val extractor: Extractor[OuterBoundaryIs] = extractor10(apply) ^^ "extractorOuterBoundaryIs"
    implicit val renderer: Renderer[OuterBoundaryIs] = renderer1(apply) ^^ "rendererOuterBoundaryIs"
}

/**
 * [[https://developers.google.com/kml/documentation/kmlreference#innerboundaryis InnerBoundaryIs]]
 *
 * @param LinearRing the linear ring which makes up this inner boundary.
 */
case class InnerBoundaryIs(LinearRing: LinearRing)

/**
 * Companion class to InnerBoundaryIs.
 */
object InnerBoundaryIs extends Extractors with Renderers {
    implicit val extractor: Extractor[InnerBoundaryIs] = extractor10(apply) ^^ "extractorInnerBoundaryIs"
    implicit val extractorSeq: MultiExtractor[Seq[InnerBoundaryIs]] = multiExtractorBase[InnerBoundaryIs](NonNegative) ^^ "multiExtractorInnerBoundaryIs"
    implicit val renderer: Renderer[InnerBoundaryIs] = renderer1(apply) ^^ "rendererInnerBoundaryIs"
    implicit val rendererSeq: Renderer[Seq[InnerBoundaryIs]] = sequenceRenderer[InnerBoundaryIs] ^^ "rendererInnerBoundaryIs"
}

/**
 * Case class LineString which extends Geometry.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#linearring LinearRing]]
 *
 * @param maybeTessellate  the (optional) tessellation.
 * @param coordinates a sequence of Coordinates objects.
 */
case class LinearRing(maybeTessellate: Option[Tessellate], coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry

object LinearRing extends Extractors with Renderers {
    private val extractorPartial: Extractor[GeometryData => LinearRing] = extractorPartial11(apply)
    implicit val extractor: Extractor[LinearRing] = extractorPartial[GeometryData, LinearRing](extractorPartial) ^^ "extractorLinearRing"
    implicit val renderer: Renderer[LinearRing] = renderer2Super(apply)(_.geometryData) ^^ "rendererLinearRing"
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
case class Style(Styles: Seq[SubStyle])(val styleSelectorData: StyleSelectorData) extends StyleSelector

object Style extends Extractors with Renderers {
    val extractorPartial: Extractor[StyleSelectorData => Style] = extractorPartial01(apply) ^^ "extractorSSD2Style"
    implicit val extractor: Extractor[Style] = extractorPartial[StyleSelectorData, Style](extractorPartial) ^^ "extractorStyle"
    implicit val renderer: Renderer[Style] = renderer1Super(apply)(_.styleSelectorData) ^^ "rendererStyle"
    implicit val rendererSeq: Renderer[Seq[Style]] = sequenceRenderer[Style] ^^ "rendererStyles"
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

    val extractorPartial: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(apply) ^^ "extractorSSD2StyleMap"
    implicit val extractor: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorPartial) ^^ "extractorStyleMap"
    implicit val renderer: Renderer[StyleMap] = renderer1Super(apply)(_.styleSelectorData) ^^ "rendererStyleMap"
    implicit val rendererSeq: Renderer[Seq[StyleMap]] = sequenceRenderer[StyleMap] ^^ "rendererStyleMaps"
}

/**
 * case class BalloonStyle which extends ColorStyle.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#balloonstyle Balloon Style]]
 *
 * NOTE Use of the color element has been deprecated (use bgColor instead.)
 * NOTE According to the current KML reference, this object extends SubStyle or ColorStyle (it's not clear which).
 *
 * @param text             the balloon text.
 * @param maybeBgColor     optional background color (maybe it isn't optional if there's no color element).
 * @param maybeTextColor   optional text color.
 * @param maybeDisplayMode optional display mode.
 * @param colorStyleData   the (auxiliary) color style properties.
 */
case class BalloonStyle(text: Text, maybeBgColor: Option[BgColor], maybeTextColor: Option[TextColor], maybeDisplayMode: Option[DisplayMode])(val colorStyleData: ColorStyleData) extends ColorStyle

object BalloonStyle extends Extractors with Renderers {

    import Renderers._

    val extractorPartial: Extractor[ColorStyleData => BalloonStyle] = extractorPartial40(apply) ^^ "extractorCSD2BalloonStyle"
    implicit val extractor: Extractor[BalloonStyle] = extractorPartial[ColorStyleData, BalloonStyle](extractorPartial) ^^ "extractorBalloonStyle"
    implicit val renderer: Renderer[BalloonStyle] = renderer4Super(apply)(_.colorStyleData) ^^ "rendererBalloonStyle"
    implicit val rendererOpt: Renderer[Option[BalloonStyle]] = optionRenderer[BalloonStyle] ^^ "rendererOptionBalloonStyle"
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
    val extractorPartial: Extractor[ColorStyleData => ListStyle] = extractorPartial30(apply) ^^ "extractorCSD2ListStyle"
    implicit val extractor: Extractor[ListStyle] = extractorPartial[ColorStyleData, ListStyle](extractorPartial) ^^ "extractorListStyle"
    implicit val renderer: Renderer[ListStyle] = renderer3Super(apply)(_.colorStyleData) ^^ "rendererListStyle"
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
    // TODO BalloonStyle and ListStyle don't belong here according to the KML Reference. ColorStyle is a sub-class of SubStyle.
    implicit val extractorSeq: MultiExtractor[Seq[ColorStyle]] =
        multiExtractor6[ColorStyle, (BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle), BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle](
            (p1, p2, p3, p4, p5, p6) => (p1, p2, p3, p4, p5, p6), Seq("BalloonStyle", "ListStyle", "PolyStyle", "LineStyle", "IconStyle", "LabelStyle")
        ) ^^ "multiExtractorColorStyle"
    implicit val renderer: Renderer[ColorStyle] = rendererSuper6[ColorStyle, IconStyle, ListStyle, BalloonStyle, LabelStyle, LineStyle, PolyStyle] ^^ "rendererColorStyle"
    implicit val rendererSeq: Renderer[Seq[ColorStyle]] = sequenceRenderer[ColorStyle] ^^ "rendererColorStyles"
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

    val extractorPartial: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(apply) ^^ "extractorSSD2ColorStyleData"
    implicit val extractor: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorPartial) ^^ "extractorColorStyleData"
    implicit val renderer: Renderer[ColorStyleData] = renderer2Super(apply)(x => x.subStyleData) ^^ "rendererColorStyleData"
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
    val extractorPartial: Extractor[ContainerData => Folder] = extractorPartial01(apply) ^^ "extractorCD2Folder"
    implicit val extractor: Extractor[Folder] = extractorPartial(extractorPartial) ^^ "extractorFolder"
    implicit val renderer: Renderer[Folder] = renderer1Super(apply)(_.containerData) ^^ "rendererFolder"
    implicit val rendererSeq: Renderer[Seq[Folder]] = sequenceRenderer[Folder] ^^ "rendererFolders"
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
    val extractorPartial: Extractor[ContainerData => Document] = extractorPartial01(apply) ^^ "extractorCD2Document"
    implicit val extractor: Extractor[Document] = extractorPartial(extractorPartial) ^^ "extractorDocument"
    implicit val renderer: Renderer[Document] = renderer1Super(apply)(_.containerData) ^^ "rendererDocument"
    implicit val rendererSeq: Renderer[Seq[Document]] = sequenceRenderer[Document] ^^ "rendererDocuments"
}

/**
 * Case class LineStyle which extends ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#linestyle Line Style]]
 *
 * NOTE: the width is optional but I can't actually find an instance of LineStyle that doesn't have a width.
 *
 * @param maybeWidth     optional width of the line.
 * @param colorStyleData other properties.
 */
case class LineStyle(maybeWidth: Option[Width])(val colorStyleData: ColorStyleData) extends ColorStyle

object LineStyle extends Extractors with Renderers {

    val extractorPartial: Extractor[ColorStyleData => LineStyle] = extractorPartial10(apply)
    implicit val extractor: Extractor[LineStyle] = extractorPartial[ColorStyleData, LineStyle](extractorPartial) ^^ "extractorLineStyle"
    implicit val renderer: Renderer[LineStyle] = renderer1Super(apply)(_.colorStyleData) ^^ "rendererLineStyle"
    implicit val rendererOpt: Renderer[Option[LineStyle]] = optionRenderer[LineStyle] ^^ "rendererOptionLineStyle"
}

/**
 * Case class PolyStyle which extends ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#polystyle Poly Style]]
 *
 * @param maybeFill      optional value of fill.
 * @param maybeOutline   optional value of outline.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class PolyStyle(maybeFill: Option[Fill], maybeOutline: Option[Outline])(val colorStyleData: ColorStyleData) extends ColorStyle

object PolyStyle extends Extractors with Renderers {
    val extractorPartial: Extractor[ColorStyleData => PolyStyle] = extractorPartial20(apply) ^^ "extractorCSD2PolyStyle"
    implicit val extractor: Extractor[PolyStyle] = extractorPartial[ColorStyleData, PolyStyle](extractorPartial) ^^ "extractorPolyStyle"
    implicit val renderer: Renderer[PolyStyle] = renderer2Super(apply)(_.colorStyleData) ^^ "rendererPolyStyle"
}

/**
 * Case class to model IconStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#iconstyle IconStyle]]
 *
 * @param maybeScale     optional Scale.
 * @param Icon           the Icon.
 * @param maybeHotSpot   optional HotSpot
 * @param maybeHeading   an optional Heading.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class IconStyle(maybeScale: Option[Scale], Icon: Icon, maybeHotSpot: Option[HotSpot], maybeHeading: Option[Heading])(val colorStyleData: ColorStyleData) extends ColorStyle

object IconStyle extends Extractors with Renderers {
    val extractorPartial: Extractor[ColorStyleData => IconStyle] = extractorPartial40(apply) ^^ "extractorCSP2IconStyle"
    implicit val extractor: Extractor[IconStyle] = extractorPartial[ColorStyleData, IconStyle](extractorPartial) ^^ "extractorIconStyle"
    implicit val renderer: Renderer[IconStyle] = renderer4Super(apply)(x => x.colorStyleData) ^^ "rendererIconStyle"
    implicit val rendererOpt: Renderer[Option[IconStyle]] = optionRenderer[IconStyle] ^^ "rendererOptionIconStyle"
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
    val extractorPartial: Extractor[ColorStyleData => LabelStyle] = extractorPartial10(apply) ^^ "extractorCSD2LabelStyle"
    implicit val extractor: Extractor[LabelStyle] = extractorPartial[ColorStyleData, LabelStyle](extractorPartial) ^^ "extractorLabelStyle"
    implicit val renderer: Renderer[LabelStyle] = renderer1Super(apply)(_.colorStyleData) ^^ "rendererLabelStyle"
    implicit val rendererOpt: Renderer[Option[LabelStyle]] = optionRenderer[LabelStyle] ^^ "rendererOptionLabelStyle"
}

//================Classes which do not appear at top level of KML Reference================================================

/**
 * Class Tessellate which is a Boolean.
 *
 * CONSIDER making this part of GeometryData.
 *
 * CONSIDER making the member have type Int (but mean Boolean) rather than String.
 *
 * TODO this (and similar case classes with "$") define the member as CharSequence. It should be String, unless we make a special KmlBoolean object.
 *
 * @param $ the value.
 */
case class Tessellate($: CharSequence)

object Tessellate extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Tessellate] = extractor10(apply) ^^ "extractorTessellate"
    implicit val extractorOpt: Extractor[Option[Tessellate]] = extractorOption[Tessellate]
    implicit val renderer: Renderer[Tessellate] = renderer1(apply) ^^ "rendererTessellate"
    implicit val rendererOpt: Renderer[Option[Tessellate]] = optionRenderer[Tessellate]
}

/**
 * Class Extrude which is a Boolean.
 *
 * Similar to Tessellate--and in fact they often go together.
 *
 * @param $ the value.
 */
case class Extrude($: CharSequence)

object Extrude extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Extrude] = extractor10(apply) ^^ "extractorExtrude"
    implicit val extractorOpt: Extractor[Option[Extrude]] = extractorOption[Extrude] ^^ "extractorOptionExtrude"
    implicit val renderer: Renderer[Extrude] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Extrude]] = optionRenderer[Extrude]
}

case class Open($: CharSequence)

object Open extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Open] = extractor10(apply)
    implicit val extractorOpt: Extractor[Option[Open]] = extractorOption[Open]
    implicit val renderer: Renderer[Open] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Open]] = optionRenderer[Open]
}

case class Visibility($: CharSequence)

object Visibility extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Visibility] = extractor10(apply)
    implicit val extractorOpt: Extractor[Option[Visibility]] = extractorOption[Visibility]
    implicit val renderer: Renderer[Visibility] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Visibility]] = optionRenderer[Visibility]
}

//
//case class Open(b: Boolean) extends KmlBoolean(b) {
//    def make(boolean: Boolean): KmlBoolean = Open(boolean)
//}
//
//object Open {
//    def create(x: Int): Open = Open(true).unit(x).asInstanceOf[Open]
//
//    import Renderers._
//
//    implicit val extractor: Extractor[Open] = intExtractor.map(x => create(x))
//    implicit val extractorOpt: Extractor[Option[Open]] = extractorOption[Open]
//    implicit val renderer: Renderer[Open] = renderer1(apply)
//    implicit val rendererOpt: Renderer[Option[Open]] = optionRenderer[Open]
//}

case class Coordinates(coordinates: Seq[Coordinate])

object Coordinates extends Extractors with Renderers {
    implicit val extractor: Extractor[Coordinates] = Extractor(node => Success(Coordinates.parse(node.text))) ^^ "extractorCoordinates"
    implicit val extractorSeq: MultiExtractor[Seq[Coordinates]] = multiExtractorBase[Coordinates](Positive) ^^ "multiExtractorCoordinates"
    implicit val renderer: Renderer[Coordinates] = renderer1(apply) ^^ "rendererCoordinates"
    implicit val rendererSeq: Renderer[Seq[Coordinates]] = sequenceRendererFormatted[Coordinates](FormatXML(_)) ^^ "rendererCoordinates_s"

    def parse(w: String): Coordinates = Coordinates((for (line <- Source.fromString(w).getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
}

/**
 * Case class Coordinate to represent a three-dimensional point.
 *
 * @param long longitude.
 * @param lat  latitude.
 * @param alt  altitude.
 */
case class Coordinate(long: String, lat: String, alt: String)

/**
 * Companion object to Coordinate.
 */
object Coordinate {

    // CONSIDER using Parser-combinators here.
    private val longLatAlt: Regex = """^\s*([\d\-\.]+),\s*([\d\-\.]+),\s*([\d\-\.]+)\s*$""".r
//    private val longLatAlt: Regex = """^\s*(((-)?(\d+(\.\d*)?)),\s*((-)?(\d+(\.\d*)?)),\s*((-)?(\d+(\.\d*)?)))\s*""".r

    def apply(w: String): Coordinate = w match {
        case longLatAlt(long, lat, alt) => Coordinate(long, lat, alt)
        case _ => throw XmlException(s"""bad coordinate string: "$w" """)
    }

    implicit val renderer: Renderer[Coordinate] = Renderer { (t: Coordinate, _: Format, _: StateR) => Success(s"${t.long},${t.lat},${t.alt}") } ^^ "rendererCoordinate"
    implicit val rendererSeq: Renderer[Seq[Coordinate]] = sequenceRendererFormatted[Coordinate](KmlRenderers.FormatCoordinate) ^^ "rendererCoordinates1"
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
    implicit val extractorOpt: Extractor[Option[Fill]] = extractorOption[Fill] ^^ "extractMaybeFill"
    implicit val renderer: Renderer[Fill] = renderer1(apply) ^^ "rendererFill"
    implicit val rendererOpt: Renderer[Option[Fill]] = optionRenderer[Fill] ^^ "rendererOptionFill"
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
    implicit val extractorOpt: Extractor[Option[Outline]] = extractorOption[Outline] ^^ "extractMaybeOutline"
    implicit val renderer: Renderer[Outline] = renderer1(apply) ^^ "rendererOutline"
    implicit val rendererOpt: Renderer[Option[Outline]] = optionRenderer[Outline] ^^ "rendererOptionOutline"
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
    implicit val extractorOpt: Extractor[Option[Heading]] = extractorOption[Heading] ^^ "extractMaybeHeading"
    implicit val renderer: Renderer[Heading] = renderer1(apply) ^^ "rendererHeading"
    implicit val rendererOpt: Renderer[Option[Heading]] = optionRenderer[Heading] ^^ "rendererOptionHeading"
}

case class BgColor($: CharSequence)

object BgColor extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[BgColor] = extractor10(apply) ^^ "extractorBgColor"
    implicit val extractorOpt: Extractor[Option[BgColor]] = extractorOption[BgColor] ^^ "extractorMaybeBgColor"
    implicit val renderer: Renderer[BgColor] = renderer1(apply) ^^ "rendererBgColor"
    implicit val rendererOpt: Renderer[Option[BgColor]] = optionRenderer[BgColor] ^^ "rendererOptionBgColor"
}

/**
 * TextColor
 * Used by BalloonStyle.
 *
 * @param $ the color.
 */
case class TextColor($: CharSequence)

object TextColor extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[TextColor] = extractor10(apply) ^^ "extractorTextColor"
    implicit val extractorOpt: Extractor[Option[TextColor]] = extractorOption[TextColor] ^^ "extractMaybeTextColor"
    implicit val renderer: Renderer[TextColor] = renderer1(apply) ^^ "rendererTextColor"
    implicit val rendererOpt: Renderer[Option[TextColor]] = optionRenderer[TextColor] ^^ "rendererOptionTextColor"
}

/**
 * DisplayMode which has values "default" or "hide".
 * Used by BalloonStyle.
 *
 * @param $ the mode.
 */
case class DisplayMode($: CharSequence)

object DisplayMode extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[DisplayMode] = extractor10(apply) ^^ "extractorDisplayMode"
    implicit val extractorOpt: Extractor[Option[DisplayMode]] = extractorOption[DisplayMode] ^^ "extractMaybeDisplayMode"
    implicit val renderer: Renderer[DisplayMode] = renderer1(apply) ^^ "rendererDisplayMode"
    implicit val rendererOpt: Renderer[Option[DisplayMode]] = optionRenderer[DisplayMode] ^^ "rendererOptionDisplayMode"
}

/**
 * ListItemType
 * CONSIDER this should be an enumerated type with values: check,checkOffOnly,checkHideChildren,radioFolder
 *
 * @param $ the value.
 */
case class ListItemType($: CharSequence)

object ListItemType extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[ListItemType] = extractor10(apply) ^^ "extractorListItemType"
    implicit val extractorOpt: Extractor[Option[ListItemType]] = extractorOption[ListItemType] ^^ "extractMaybeListItemType"
    implicit val renderer: Renderer[ListItemType] = renderer1(apply) ^^ "rendererListItemType"
    implicit val rendererOpt: Renderer[Option[ListItemType]] = optionRenderer[ListItemType] ^^ "rendererOptionListItemType"
}

/**
 * State
 * CONSIDER this should be an enumerated type with values: open, closed, error, fetching0, fetching1, or fetching2
 *
 * @param $ the value.
 */
case class State($: CharSequence)

object State extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[State] = extractor10(apply) ^^ "extractorState"
    implicit val renderer: Renderer[State] = renderer1(apply) ^^ "rendererState"
    implicit val rendererOpt: Renderer[Option[State]] = optionRenderer[State] ^^ "rendererOptionState"
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
    implicit val extractorOpt: Extractor[Option[ItemIcon]] = extractorOption[ItemIcon] ^^ "extractMaybeItemIcon"
    implicit val renderer: Renderer[ItemIcon] = renderer2(apply) ^^ "rendererItemIcon"
    implicit val rendererOpt: Renderer[Option[ItemIcon]] = optionRenderer[ItemIcon] ^^ "rendererOptionItemIcon"
}

/**
 * Case class to model a HotSpot.
 *
 * TODO change _xunits and _yunits to be of type String.
 *
 * @param _x      optional x field.
 * @param _xunits optional xunits field.
 * @param _y      optional y field.
 * @param _yunits optional yunits field.
 */
case class HotSpot(_x: Int, _xunits: CharSequence, _y: Int, _yunits: CharSequence)

object HotSpot extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[HotSpot] = extractor40(apply) ^^ "extractorHotspot"
    implicit val extractorOpt: Extractor[Option[HotSpot]] = extractorOption[HotSpot] ^^ "extractMaybeHotSpot"
    implicit val renderer: Renderer[HotSpot] = renderer4(apply) ^^ "rendererHotSpot"
    implicit val rendererOpt: Renderer[Option[HotSpot]] = optionRenderer[HotSpot] ^^ "rendererOptionHotSpot"
}

/**
 * Case class to define Color.  An element of ColorStyle.
 * See [[https://developers.google.com/kml/documentation/kmlreference#colorstyle ColorStyle]]
 *
 * @param $ the color as a hexadecimal string.
 */
case class Color($: CharSequence)

object Color extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Color] = extractor10(apply) ^^ "extractorColor"
    implicit val extractorOpt: Extractor[Option[Color]] = extractorOption[Color] ^^ "extractMaybeColor"
    implicit val renderer: Renderer[Color] = renderer1(apply) ^^ "rendererColor"
    implicit val rendererOpt: Renderer[Option[Color]] = optionRenderer[Color] ^^ "rendererOptionColor"
}

/**
 * Case class to model ColorMode.
 * See [[https://developers.google.com/kml/documentation/kmlreference#colormode ColorMode]]
 *
 * @param $ the color mode as string: "normal" or "random."
 */
case class ColorMode($: CharSequence)

object ColorMode extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[ColorMode] = extractor10(apply) ^^ "extractorColorMode"
    implicit val extractorOpt: Extractor[Option[ColorMode]] = extractorOption[ColorMode] ^^ "extractMaybeColorMode"
    implicit val renderer: Renderer[ColorMode] = renderer1(apply) ^^ "rendererColorMode"
    implicit val rendererOpt: Renderer[Option[ColorMode]] = optionRenderer[ColorMode] ^^ "rendererOptionColorMode"
}

case class Width($: Double)

object Width extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Width] = extractor10(apply) ^^ "extractorWidth"
    implicit val extractorOpt: Extractor[Option[Width]] = extractorOption[Width] ^^ "extractMaybeWidth"
    implicit val renderer: Renderer[Width] = renderer1(apply) ^^ "rendererWidth"
    implicit val rendererOpt: Renderer[Option[Width]] = optionRenderer[Width] ^^ "rendererOptionWidth"
}

case class Key($: CharSequence)

object Key extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Key] = extractor10(apply) ^^ "extractorKey"
    implicit val renderer: Renderer[Key] = renderer1(apply) ^^ "rendererKey"
}

case class StyleURL($: CharSequence)

object StyleURL extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[StyleURL] = extractor10(apply) ^^ "extractorStyleURL"
    implicit val renderer: Renderer[StyleURL] = renderer1(apply) ^^ "rendererStyleURL"
}

case class Pair(key: Key, styleUrl: StyleURL)

object Pair extends Extractors with Renderers {

    implicit val extractor: Extractor[Pair] = extractor20(apply) ^^ "extractorPair"
    implicit val extractorSeq: MultiExtractor[Seq[Pair]] = multiExtractorBase[Pair](Positive) ^^ "multiExtractorPair"
    implicit val renderer: Renderer[Pair] = renderer2(apply) ^^ "rendererPair"
    implicit val rendererSeq: Renderer[Seq[Pair]] = sequenceRenderer[Pair] ^^ "rendererPairs"
}


case class Longitude($: Double)

object Longitude extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Longitude] = extractor10(apply)
    implicit val extractorOpt: Extractor[Option[Longitude]] = extractorOption[Longitude]
    implicit val renderer: Renderer[Longitude] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Longitude]] = optionRenderer[Longitude]
}


case class Latitude($: Double)

object Latitude extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Latitude] = extractor10(apply)
    implicit val extractorOpt: Extractor[Option[Latitude]] = extractorOption[Latitude]
    implicit val renderer: Renderer[Latitude] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Latitude]] = optionRenderer[Latitude]
}

case class Altitude($: Double)

object Altitude extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Altitude] = extractor10(apply)
    implicit val extractorOpt: Extractor[Option[Altitude]] = extractorOption[Altitude]
    implicit val renderer: Renderer[Altitude] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Altitude]] = optionRenderer[Altitude]
}

case class Tilt($: Double)

object Tilt extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Tilt] = extractor10(apply)
    implicit val extractorOpt: Extractor[Option[Tilt]] = extractorOption[Tilt]
    implicit val renderer: Renderer[Tilt] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[Tilt]] = optionRenderer[Tilt]
}

case class Range($: Double)

object Range extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Range] = extractor10(apply)
    implicit val renderer: Renderer[Range] = renderer1(apply)
}

case class Roll($: Double)

object Roll extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[Roll] = extractor10(apply)
    implicit val renderer: Renderer[Roll] = renderer1(apply)
}

case class AltitudeMode($: CharSequence)

object AltitudeMode extends Extractors with Renderers {

    import Renderers._

    implicit val extractor: Extractor[AltitudeMode] = extractor10(apply) ^^ "extractorAltitudeMode"
    implicit val extractorOpt: Extractor[Option[AltitudeMode]] = extractorOption[AltitudeMode] ^^ "extractorOptAltitudeMode"
    implicit val renderer: Renderer[AltitudeMode] = renderer1(apply)
    implicit val rendererOpt: Renderer[Option[AltitudeMode]] = optionRenderer[AltitudeMode]
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
    def init(): Unit = {
        TagProperties.addMustMatch("innerBoundaryIs")
    }

    implicit val extractor: Extractor[KML] = extractor01(apply) ^^ "extractorKml"
    implicit val extractorSeq: MultiExtractor[Seq[KML]] = multiExtractorBase[KML](Positive) ^^ "multiExtractorKml"
    implicit val renderer: Renderer[KML] = renderer1(apply) ^^ "rendererKml"
}

case class KML_Binding(kml: KML, binding: NamespaceBinding)

object KML_Binding {
    implicit val extractor: Extractor[KML_Binding] = Extractor {
        node =>
            Extractor.extract[KML](node) map (KML_Binding(_, node.scope))
    }
    implicit val renderer: Renderer[KML_Binding] = Renderer {
        (t: KML_Binding, format: Format, stateR: StateR) =>
            TryUsing(stateR.addAttribute(s"""${t.binding}"""))(rs => Renderer.render(t.kml, format, rs))
    } ^^ "rendererKml_Binding"
}

object KmlRenderers extends Renderers {

    case class FormatCoordinate(indents: Int) extends BaseFormat(indents) {
      val name: String = "formatCoordinate"

        def indent: Format = this

      def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = open match {
          case Some(true) =>
              ""
          case _ =>
              ""
      }

      def sequencer(open: Option[Boolean]): String = open match {
          case Some(true) => newline
          case Some(false) => ""
          case _ => "\n"
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

//abstract class KmlBoolean(b: Boolean) {
//    override def toString: String = if (b) "1" else "0"
//
//    def make(boolean: Boolean): KmlBoolean
//
//    def unit(x: Int): KmlBoolean = make(x match {case 1 => true; case _ => false})
//}
