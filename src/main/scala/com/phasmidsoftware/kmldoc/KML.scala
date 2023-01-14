package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.core.{Text, TryUsing, XmlException}
import com.phasmidsoftware.kmldoc.KmlData.{extractorKmlData, rendererKmlData}
import com.phasmidsoftware.kmldoc.KmlExtractors.{extractor0, extractor01, extractor10, extractor11, extractor20, extractor40, extractorAlia6, extractorAlt, extractorCD2Document, extractorCD2Folder, extractorCSD2BalloonStyle, extractorCSD2LabelStyle, extractorCSD2LineStyle, extractorCSD2ListStyle, extractorCSD2PolyStyle, extractorCSP2IconStyle, extractorFD2ContainerData, extractorFD2Placemark, extractorGD2Point, extractorKD2FD, extractorKD2GeometryData, extractorKD2Scale, extractorKD2StyleSelectorData, extractorKD2SubStyleData, extractorPartial, extractorSSD2ColorStyleData, extractorSSD2Style, extractorSSD2StyleMap, multiExtractorCoordinates, multiExtractorFeature}
import com.phasmidsoftware.kmldoc.KmlRenderers.{renderer0Super, renderer1, renderer1Super, renderer2, renderer2Super, renderer3Super, renderer4, renderer4Super, renderer5Super, rendererColorStyles, rendererCoordinates1, rendererCoordinates_s, rendererFeatures, rendererGeometrys, rendererOptionBgColor, rendererOptionColor, rendererOptionColorMode, rendererOptionHeading, rendererOptionItemIcon, rendererOptionListItemType, rendererOptionTextColor, rendererSequencePair, rendererSuper2, rendererSuper6, sequenceRenderer}
import com.phasmidsoftware.render.Renderers.{doubleRenderer, intRenderer, rendererOptionInt, rendererOptionString, rendererOptionText, rendererText, stringRenderer}
import com.phasmidsoftware.render._
import com.phasmidsoftware.xml.Extractor.none
import com.phasmidsoftware.xml.Extractors.{StringExtractor, doubleExtractor, extractorText, intExtractor}
import com.phasmidsoftware.xml._
import java.net.URL
import org.slf4j.{Logger, LoggerFactory}
import scala.io.Source
import scala.reflect.{ClassTag, classTag}
import scala.util.matching.Regex
import scala.util.{Success, Try}
import scala.xml.{Elem, NamespaceBinding, Node, XML}

/**
 * Abstract super-element of all KML elements. Known in the reference document as Object.
 * See [[https://developers.google.com/kml/documentation/kmlreference KML Reference]]
 *
 * NOTE: abstract elements cannot be represented in Kml documents directly but are always super-elements (and therefore ordinary classes).
 * Their usage is in the properties of other elements where more than one possible element type is allowed.
 *
 * TODO remove explicit implicit parameters
 *
 * Actual Kml elements (the leaf classes) are all represented here as case classes, which cannot be extended.
 */
class KmlObject {
//  val subClasses: Seq[Class[_]] = Seq(classOf[Feature], classOf[geometrys], classOf[StyleSelector], classOf[SubStyle], classOf[Scale])
}

/**
 * Companion object to KmlObject.
 */
object KmlObject {
//  implicit object loggableKmlObject extends LoggableAny[KmlObject]
}

/**
 * Properties of KMLObject
 *
 * @param __id an optional identifier.
 */
case class KmlData(__id: Option[String])

/**
 * Companion object to KmlData.
 */
object KmlData {

    import Extractors.extractorOptionalString
    implicit val extractorKmlData: Extractor[KmlData] = extractor10(apply)(extractorOptionalString, classTag) ^^ "extractorKmlData"

    implicit lazy val rendererKmlData: Renderable[KmlData] = renderer1(apply) ^+ "rendererKmlData"

    def nemo: KmlData = KmlData(None)

//  implicit object loggableKmlData extends LoggableAny[KmlData]
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
object Scale {
    import KmlData.extractorKmlData
    implicit lazy val extractorScale: Extractor[Scale] = extractorPartial[KmlData, Scale](extractorKD2Scale)(extractorKmlData, classTag) ^^ "extractorScale"
    implicit val rendererScale: Renderable[Scale] = renderer1Super(apply)(_.kmlData) ^+ "rendererScale"

    def nemo(x: Double): Scale = new Scale(x)(KmlData.nemo)
}

/**
 * Abstract Feature element.
 * Feature is a sub-type of Object and a super-type of Placemark, Container.
 * See [[https://developers.google.com/kml/documentation/kmlreference#feature Feature]].
 *
 * TODO add Overlay, NetworkLink.
 */
class Feature extends KmlObject

/**
 * Companion object to Feature.
 */
object Feature {
    // NOTE we don't currently use extractorFeature.
    // CONSIDER should we use it instead of the mechanism in, for example, multiExtractor2.
//    implicit lazy  val extractorFeature: Extractor[Feature] = extractorAlt[Feature,Container,Placemark](Container.extractorContainer,Placemark.extractorPlacemark)
    implicit  val rendererFeature: Renderable[Feature] = rendererSuper2[Feature, Placemark, Container] ^+ "rendererFeature"
//    val applyFunction: Unit => Feature = _ => new Feature() // CONSIDER do we need this?
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
case class FeatureData(name: Text, maybeDescription: Option[Text], maybeStyleUrl: Option[Text], maybeOpen: Option[Int], StyleSelectors: Seq[StyleSelector])( val kmlData: KmlData)

/**
 * Companion object to Feature.
 */
object FeatureData {
    import KmlData.extractorKmlData
    implicit lazy val extractorFeatureData: Extractor[FeatureData] = extractorPartial[KmlData, FeatureData](extractorKD2FD)(extractorKmlData, classTag) ^^ "extractorFeatureData"
    import KmlRenderers.rendererStyleSelectors
    implicit lazy val rendererFeatureData: Renderable[FeatureData] = renderer5Super(apply)(_.kmlData) ^+ "rendererFeatureData"
}

/**
 * Placemark: sub-type of Feature.
 * See [[https://developers.google.com/kml/documentation/kmlreference#placemark Placemark]].
 *
 * @param geometrys    a sequence of geometrys elements (where geometrys is an abstract super-type).
 * @param featureData the (auxiliary) FeatureData, shared by sub-elements.
 */
case class Placemark(geometrys: Seq[Geometry])(val featureData: FeatureData) extends Feature

/**
 * Companion object to Placemark.
 */
object Placemark {
    import FeatureData.extractorFeatureData
     lazy val extractorPlacemark: Extractor[Placemark] = extractorPartial[FeatureData, Placemark](extractorFD2Placemark)(extractorFeatureData, classTag) ^^ "extractorPlacemark"
    implicit lazy val rendererPlacemark: Renderable[Placemark] = renderer1Super(apply)(_.featureData) ^+ "rendererPlacemark"
}

/**
 * Abstract Container element.
 * Container is a sub-type of Feature and a super-type of Folder, Document.
 * See [[https://developers.google.com/kml/documentation/kmlreference#container Container]].
 *
 * A Container has no properties of its own.
 */
class Container extends Feature

/**
 * Companion object to Container.
 */
object Container {
    implicit  val extractorContainer: Extractor[Container] = extractor0(applyFunction) ^^ "extractorContainer"
    implicit  val rendererContainer: Renderable[Container] = rendererSuper2[Container, Folder, Document] ^+ "rendererContainer"
    lazy val applyFunction: Unit => Container = _ => new Container()
}

/**
 * Properties of a Container (and therefore all its sub-types).
 *
 * CONSIDER having an empty member set.
 *
 * @param featureData (auxiliary) member: FeatureData, shared by sub-elements.
 */
case class ContainerData(featureData: FeatureData)

/**
 * Companion object to ContainerData.
 */
object ContainerData {
    lazy val applyFunction: FeatureData => ContainerData = new ContainerData(_)
    implicit lazy val extractorContainerData: Extractor[ContainerData] = extractorPartial[FeatureData, ContainerData](extractorFD2ContainerData)(FeatureData.extractorFeatureData, classTag) ^^ "extractorContainerData"
    implicit lazy val rendererContainerData: Renderable[ContainerData] = renderer0Super(applyFunction)(_.featureData) ^+ "rendererContainerData"
}

/**
 * Folder: sub-element of Container.
 * See [[https://developers.google.com/kml/documentation/kmlreference#container Folder]].
 *
 * @param features      a sequence of Feature elements (where Feature is an abstract super-type).
 * @param containerData the ContainerData (auxiliary property).
 */
case class Folder(features: Seq[Feature])(val containerData: ContainerData) extends Container

/**
 * Companion object to Folder.
 */
object Folder {
    // NOTE must not be lazy (infinite recursion must be broken somewhere)
     implicit val extractorFolder: Extractor[Folder] = extractorPartial(extractorCD2Folder)(ContainerData.extractorContainerData, classTag) ^^ "extractorFolder"
    implicit lazy val rendererFolder: Renderable[Folder] = renderer1Super(apply)(_.containerData) ^+ "rendererFolder"
}

/**
 * Document: sub-element of Container.
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
object Document {
    // NOTE must not be lazy
    implicit val extractorDocument: Extractor[Document] = extractorPartial(extractorCD2Document)(ContainerData.extractorContainerData, classTag) ^^ "extractorDocument"
    implicit lazy val rendererDocument: Renderable[Document] = renderer1Super(apply)(_.containerData) ^+ "rendererDocument"
}

/**
 * geometrys: abstract sub-element of KmlObject.
 * [[https://developers.google.com/kml/documentation/kmlreference#geometry geometrys]]
 */
class Geometry extends KmlObject

/**
 * Companion object to Document.
 */
object Geometry {
//    implicit  val extractorGeometry: Extractor[Geometry] = extractorAlt[Geometry, LineString, Point](LineString.extractorLineString, Point.extractorPoint) ^^ "extractorGeometry"
    implicit lazy val rendererGeometry: Renderable[Geometry] = rendererSuper2[Geometry, Point, LineString] ^+ "rendererGeometry"
}

case class GeometryData(kmlData: KmlData)

object GeometryData {
    implicit lazy val extractorGeometryData: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorKD2GeometryData) ^^ "extractorGeometryData"
    implicit lazy val rendererGeometryData: Renderable[GeometryData] = renderer0Super(apply)(_.kmlData) ^+ "rendererGeometryData"

    lazy val applyFunction: KmlData => GeometryData = new GeometryData(_)
}

case class Point(coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry

object Point {
    implicit lazy val extractorPoint: Extractor[Point] = extractorPartial[GeometryData, Point](extractorGD2Point)(GeometryData.extractorGeometryData, classTag) ^^ "extractorPoint"
    implicit lazy val rendererPoint: Renderable[Point] = renderer1Super(apply)(_.geometryData) ^+ "rendererPoint"
}

/**
 * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
 */
class StyleSelector extends KmlObject

object StyleSelector {
//    implicit val extractorStyleSelector: Extractor[StyleSelector] = extractorAlt[StyleSelector, Style, StyleMap](Style.extractorStyle, StyleMap.extractorStyleMap) ^^ "extractorStyleSelector"
    implicit  val rendererStyleSelector: Renderable[StyleSelector] = rendererSuper2[StyleSelector, Style, StyleMap] ^+ "rendererStyleSelector"
}

case class StyleSelectorData(kmlData: KmlData)

object StyleSelectorData {
    lazy val applyFunction: KmlData => StyleSelectorData = new StyleSelectorData(_)
    implicit lazy  val extractorStyleSelectorData: Extractor[StyleSelectorData] = extractorPartial[KmlData, StyleSelectorData](extractorKD2StyleSelectorData)(extractorKmlData, classTag) ^^ "extractorStyleSelectorData"
    implicit lazy val rendererStyleSelectorData: Renderable[StyleSelectorData] = renderer1(apply) ^+ "rendererStyleSelectorData"
}

/**
 * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
 */
class SubStyle extends KmlObject

case class SubStyleData(kmlData: KmlData)

object SubStyleData {
    implicit lazy  val extractorSubStyleData: Extractor[SubStyleData] = extractorPartial[KmlData, SubStyleData](extractorKD2SubStyleData)(extractorKmlData, classTag) ^^ "extractorSubStyleData"
    private val fKP2SSP: KmlData => SubStyleData = k => SubStyleData(k)
    implicit lazy val rendererSubStyleData: Renderable[SubStyleData] = renderer0Super(apply)(_.kmlData) ^+ "rendererSubStyleData"

    lazy val applyFunction: KmlData => SubStyleData = SubStyleData.apply
}

// NOTE: ColorStyle is good
class ColorStyle extends SubStyle

object ColorStyle {
    implicit lazy  val extractorColorStyle: Extractor[ColorStyle] =
        extractorAlia6[ColorStyle, BalloonStyle, LineStyle, IconStyle, ListStyle, PolyStyle, LabelStyle](BalloonStyle.extractorBalloonStyle, LineStyle.extractorLineStyle, IconStyle.extractorIconStyle, ListStyle.extractorListStyle, PolyStyle.extractorPolyStyle, LabelStyle.extractorLabelStyle) ^^
                "extractorColorStyle"
    implicit  val rendererColorStyle: Renderable[ColorStyle] = rendererSuper6[ColorStyle, IconStyle, ListStyle, BalloonStyle, LabelStyle, LineStyle, PolyStyle] ^+ "rendererColorStyle"
}

case class ColorStyleData(maybeColor: Option[Color], maybeColorMode: Option[ColorMode])(val subStyleData: SubStyleData)

object ColorStyleData {
    implicit lazy val extractorColorStyleData: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorSSD2ColorStyleData)(SubStyleData.extractorSubStyleData, classTag) ^^ "extractorColorStyleData"
    implicit  val rendererColorStyleData: Renderable[ColorStyleData] = renderer2Super(ColorStyleData.apply)(x => x.subStyleData) ^+ "rendererColorStyleData"
}

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

object BalloonStyle {
    implicit lazy  val extractorBalloonStyle: Extractor[BalloonStyle] = extractorPartial[ColorStyleData, BalloonStyle](extractorCSD2BalloonStyle)(ColorStyleData.extractorColorStyleData, classTag) ^^ "extractorBalloonStyle"
    implicit lazy val rendererBalloonStyle: Renderable[BalloonStyle] = renderer4Super(apply)(_.colorStyleData) ^+ "rendererBalloonStyle"
}

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

object ListStyle {
    implicit lazy  val extractorListStyle: Extractor[ListStyle] = extractorPartial[ColorStyleData, ListStyle](extractorCSD2ListStyle)(ColorStyleData.extractorColorStyleData, classTag) ^^ "extractorListStyle"
    implicit lazy val rendererListStyle: Renderable[ListStyle] = renderer3Super(apply)(_.colorStyleData) ^+ "rendererListStyle"
}

/**
 * LineStyle
 *
 * @param width          the line width
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class LineStyle(width: Width)(val colorStyleData: ColorStyleData) extends ColorStyle

object LineStyle {
    implicit lazy  val extractorLineStyle: Extractor[LineStyle] = extractorPartial[ColorStyleData, LineStyle](extractorCSD2LineStyle)(ColorStyleData.extractorColorStyleData, classTag) ^^ "extractorLineStyle"
    implicit lazy val rendererLineStyle: Renderable[LineStyle] = renderer1Super(apply)(_.colorStyleData) ^+ "rendererLineStyle"
}

/**
 * PolyStyle
 *
 * @param fill           the value of fill.
 * @param outline        the value of outline.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class PolyStyle(fill: Fill, outline: Outline)(val colorStyleData: ColorStyleData) extends ColorStyle

object PolyStyle {
    implicit lazy  val extractorPolyStyle: Extractor[PolyStyle] = extractorPartial[ColorStyleData, PolyStyle](extractorCSD2PolyStyle)(ColorStyleData.extractorColorStyleData, classTag) ^^ "extractorPolyStyle"
    implicit lazy val rendererPolyStyle: Renderable[PolyStyle] = renderer2Super(apply)(_.colorStyleData) ^+ "rendererPolyStyle"
}

/**
 * LabelStyle
 *
 * @param scale          the scale.
 * @param colorStyleData the (auxiliary) color style properties.
 */
case class LabelStyle(scale: Scale)(val colorStyleData: ColorStyleData) extends ColorStyle

object LabelStyle {
    implicit lazy  val extractorLabelStyle: Extractor[LabelStyle] = extractorPartial[ColorStyleData, LabelStyle](extractorCSD2LabelStyle)(ColorStyleData.extractorColorStyleData, classTag) ^^ "extractorLabelStyle"
    implicit lazy val rendererLabelStyle: Renderable[LabelStyle] = renderer1Super(apply)(_.colorStyleData) ^+ "rendererLabelStyle"
}

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

object IconStyle {
    implicit lazy  val extractorIconStyle: Extractor[IconStyle] = extractorPartial[ColorStyleData, IconStyle](extractorCSP2IconStyle)(ColorStyleData.extractorColorStyleData, classTag) ^^ "extractorIconStyle"
    implicit lazy val rendererIconStyle: Renderable[IconStyle] = renderer4Super(IconStyle.apply)(x => x.colorStyleData) ^+ "rendererIconStyle"
}

/**
 * Fill.
 * NOTE that this is a boolean that is represented by 0 or 1.
 *
 * @param boolean whether to fill or not.
 */
case class Fill(boolean: Int)

object Fill {
    implicit   val extractorFill: Extractor[Fill] = extractor10(apply)(intExtractor, classTag) ^^ "extractorFill"
    implicit lazy val rendererFill: Renderable[Fill] = renderer1(apply) ^+ "rendererFill"
}

/**
 * Outline.
 * NOTE that this is a boolean that is represented by 0 or 1.
 *
 * @param boolean whether to outline or not.
 */
case class Outline(boolean: Int)

object Outline {
    implicit lazy  val extractorOutline: Extractor[Outline] = extractor10(apply)(intExtractor, classTag) ^^ "extractorOutline"
    implicit lazy val rendererOutline: Renderable[Outline] = renderer1(apply) ^+ "rendererOutline"
}

/**
 * Case class to represent a Heading which is represented in XML as, for example: <heading>1.1</heading>
 *
 * @param $ the value of the heading (a Double).
 */
case class Heading($: Double)

object Heading {
    implicit lazy  val extractorHeading: Extractor[Heading] = extractor10(apply)(doubleExtractor, classTag) ^^ "extractorHeading"
    implicit lazy val rendererHeading: Renderable[Heading] = renderer1(apply) ^+ "rendererHeading"
}

case class BgColor($: String)

object BgColor {
    implicit lazy  val extractorBgColor: Extractor[BgColor] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorBgColor"
    implicit lazy val rendererBgColor: Renderable[BgColor] = renderer1(apply) ^+ "rendererBgColor"
}

/**
 * TextColor
 * Used by BalloonStyle.
 *
 * @param $ the color.
 */
case class TextColor($: String)

object TextColor {
    implicit lazy  val extractorTextColor: Extractor[TextColor] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorTextColor"
    implicit lazy val rendererTextColor: Renderable[TextColor] = renderer1(apply) ^+ "rendererTextColor"
}

/**
 * DisplayMode which has values "default" or "hide".
 * Used by BalloonStyle.
 *
 * @param $ the mode.
 */
case class DisplayMode($: String)

object DisplayMode {
    implicit lazy  val extractorDisplayMode: Extractor[DisplayMode] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorDisplayMode"
    implicit lazy val rendererDisplayMode: Renderable[DisplayMode] = renderer1(apply) //^^ "rendererDisplayMode"
}

/**
 * ListItemType
 * TODO this should be an enumerated type with values: check,checkOffOnly,checkHideChildren,radioFolder
 *
 * @param $ the value.
 */
case class ListItemType($: String)

object ListItemType {
    implicit lazy  val extractorListItemType: Extractor[ListItemType] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorListItemType"
    implicit lazy val rendererListItemType: Renderable[ListItemType] = renderer1(apply) ^+ "rendererListItemType"
}

/**
 * State
 * TODO this should be an enumerated type with values: open, closed, error, fetching0, fetching1, or fetching2
 *
 * @param $ the value.
 */
case class State($: String)

object State {
    implicit lazy  val extractorState: Extractor[State] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorState"
    implicit lazy val rendererState: Renderable[State] = renderer1(apply) ^+ "rendererState"
}

/**
 * ItemIcon
 *
 * @param state the state.
 */
case class ItemIcon(state: State, href: Text)

object ItemIcon {
    implicit lazy  val extractorItemIcon: Extractor[ItemIcon] = extractor20(apply)(State.extractorState, extractorText, classTag) ^^ "extractorItemIcon"
    implicit lazy val rendererItemIcon: Renderable[ItemIcon] = renderer2(apply) ^+ "rendererItemIcon"
}

case class Icon(href: Text)

object Icon {
    implicit lazy  val extractorIcon: Extractor[Icon] = extractor10(apply)(extractorText, classTag) ^^ "extractorIcon"
    implicit lazy val rendererIcon: Renderable[Icon] = renderer1(apply) ^+ "rendererIcon"
}

case class HotSpot(_x: Int, _xunits: String, _y: Int, _yunits: String)

object HotSpot {
    implicit lazy  val extractorHotspot: Extractor[HotSpot] = extractor40(apply)(intExtractor, StringExtractor, intExtractor, StringExtractor, classTag) ^^ "extractorHotspot"
    implicit lazy val rendererHotSpot: Renderable[HotSpot] = renderer4(apply) ^+ "rendererHotSpot"
}

case class Color($: String)

object Color {
    implicit lazy  val extractorColor: Extractor[Color] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorColor"
    implicit lazy val rendererColor: Renderable[Color] = renderer1(apply) ^+ "rendererColor"
}

case class ColorMode($: String)

object ColorMode {
    implicit lazy  val extractorColorMode: Extractor[ColorMode] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorColorMode"
    implicit lazy val rendererColorMode: Renderable[ColorMode] = renderer1(apply) ^+ "rendererColorMode"
}

case class Width($: Double)

object Width {
    implicit lazy  val extractorWidth: Extractor[Width] = extractor10(apply)(doubleExtractor, classTag) ^^ "extractorWidth"
    implicit lazy val rendererWidth: Renderable[Width] = renderer1(apply) ^+ "rendererWidth"
}

/**
 * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
 */

/**
 * Case class to define a KML object.
 *
 * NOTE WELL: do not be tempted to add "_xmlns" as a member.
 * If you do, you will run into the undocumented(?) "feature" of the Xml library that "xmlns" is a reserved attribute name.
 *
 * @param features a sequence of Feature (typically Document).
 */
// TODO add in the xmlns tag (a top-level attribute)
case class KML(features: Seq[Feature])

object KML {
    lazy  val extractorKml: Extractor[KML] = extractor01(apply)(multiExtractorFeature, classTag[KML]) ^^ "extractorKml"
    implicit lazy val rendererKml: Renderable[KML] = renderer1(apply) ^+ "rendererKml"
}

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

object Style {
    implicit lazy  val extractorStyle: Extractor[Style] = extractorPartial[StyleSelectorData, Style](extractorSSD2Style)(StyleSelectorData.extractorStyleSelectorData, classTag)
    implicit lazy val rendererStyle: Renderable[Style] = renderer1Super(apply)(_.styleSelectorData) ^+ "rendererStyle"
}

case class Pair(key: String, styleUrl: String)

object Pair {

    implicit lazy  val extractorPair: Extractor[Pair] = extractor20(Pair.apply)(StringExtractor, StringExtractor, classTag) ^^ "multiExtractorPair"
    implicit lazy val rendererPair: Renderable[Pair] = renderer2(Pair.apply) ^+ "rendererPair"
}

case class StyleMap(Pairs: Seq[Pair])(val styleSelectorData: StyleSelectorData) extends StyleSelector

object StyleMap {
    import StyleSelectorData.extractorStyleSelectorData
    lazy  val extractorStyleMap: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorSSD2StyleMap)(StyleSelectorData.extractorStyleSelectorData, classTag)
    implicit lazy val rendererStyleMap: Renderable[StyleMap] = renderer1Super(apply)(_.styleSelectorData) ^+ "rendererStyleMap"
}

case class Tessellate($: String)

object Tessellate {
    implicit lazy  val extractorTessellate: Extractor[Tessellate] = extractor10(apply)(StringExtractor, classTag) ^^ "extractorTessellate"
    implicit lazy val rendererTessellate: Renderable[Tessellate] = renderer1(apply) ^+ "rendererTessellate"
}

case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates]) extends Geometry

object LineString {
    import com.phasmidsoftware.kmldoc.KmlExtractors.multiExtractorCoordinates
    implicit lazy val extractorLineString: Extractor[LineString] = extractor11(LineString.apply)(Tessellate.extractorTessellate, multiExtractorCoordinates, classTag[LineString]) ^^ "extractorLineString"
    implicit lazy val rendererLineString: Renderable[LineString] = renderer2(apply) ^+ "rendererLineString"
}

case class Coordinates(coordinates: Seq[Coordinate])

object Coordinates {
    implicit lazy  val extractorCoordinates: Extractor[Coordinates] = Extractor((node: Node) => Success(Coordinates.parse(node.text))) ^^ "extractorCoordinates"
    implicit lazy val rendererCoordinates: Renderable[Coordinates] = renderer1(apply) //^^ "rendererCoordinates"

    def parse(w: String): Coordinates = {
        val source = Source.fromString(w)
        Coordinates((for (line <- source.getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
    }
}

case class Coordinate(lat: String, long: String, alt: String)

object Coordinate {
    implicit lazy val rendererCoordinate: Renderable[Coordinate] = Renderable { (t: Coordinate, _: Format, _: StateR) => Success(s"${t.long}, ${t.lat}, ${t.alt}") } ^^ "rendererCoordinate"

    private val latLong: Regex = """\s*([\d\-\.]+),([\d\-\.]+),([\d\-\.]+)""".r

    def apply(w: String): Coordinate = w match {
        case latLong(long, lat, alt) => Coordinate(lat, long, alt)
        case _ => throw XmlException(s"bad coordinate string: $w")
    }
}

object KmlExtractors extends Extractors {

    Extractor.translations += "coordinates" -> Seq("coordinates")
    // TODO add Overlay, NetworkLink,
    Extractor.translations += "features" -> Seq("Placemark", "Folder", "Document")
    Extractor.translations += "geometrys" -> Seq("LineString", "Point")
    Extractor.translations += "Feature" -> Seq("_")
    Extractor.translations += "Container" -> Seq("_")
    Extractor.translations += "StyleSelector" -> Seq("_")
    Extractor.translations += "SubStyle" -> Seq("_")
    Extractor.translations += "ColorStyle" -> Seq("_")

    import Extractors._

    /**
     * The following extractors do not depend on any other extractors.
     */
     val extractorKD2GeometryData: Extractor[KmlData => GeometryData] = extractorPartial0[KmlData, GeometryData](GeometryData.applyFunction) ^^ "extractorKD2GeometryData"
     val extractorKD2SubStyleData: Extractor[KmlData => SubStyleData] = extractorPartial0[KmlData, SubStyleData](SubStyleData.applyFunction) ^^ "extractorKD2SubStyleData"
      val extractorKD2StyleSelectorData: Extractor[KmlData => StyleSelectorData] = extractorPartial0[KmlData, StyleSelectorData](StyleSelectorData.applyFunction) ^^ "extractorKD2StyleSelectorData"
      val extractorFD2ContainerData: Extractor[FeatureData => ContainerData] = extractorPartial0[FeatureData, ContainerData](ContainerData.applyFunction) ^^ "extractorFD2ContainerData"

    /**
     * The following extractors do not depend on other KML-defined extractors.
     */
    implicit val extractorKD2Scale: Extractor[KmlData => Scale] = extractorPartial10(Scale.apply)(doubleExtractor, classTag) ^^ "extractorKD2Scale"

    /**
     * The following extractors depend on other KML-defined extractors which depend on others.
     *
     * NOTE: these declarations need to be non-lazy else stack overflow: at least SOMETHING needs to break the recursion. ???
     */
    implicit lazy  val multiExtractorGeometry: MultiExtractor[Seq[Geometry]] =
    // CONSIDER passing identity instead of (l,p) => (l,p)
        multiExtractor2[Geometry, (LineString, Point), LineString, Point]((l, p) => (l, p), Seq("LineString", "Point"))(
            classTag, LineString.extractorLineString, classTag, Point.extractorPoint, classTag
        ) ^^
                "multiExtractorGeometry"

    // NOTE it works to make this val and have extractorCD2Folder, etc. as lazy val.
    implicit lazy val multiExtractorFeature: MultiExtractor[Seq[Feature]] =
        multiExtractor3[Feature, (Placemark, Folder, Document), Placemark, Folder, Document]((p, f, d) => (p, f, d), Seq("Placemark", "Folder", "Document"))(
            classTag, Placemark.extractorPlacemark, classTag, Folder.extractorFolder, classTag, Document.extractorDocument, classTag
    ) ^^ "multiExtractorFeature"

    implicit lazy  val multiExtractorContainer: MultiExtractor[Seq[Container]] =
        multiExtractor2[Container, (Folder, Document), Folder, Document]((f, d) => (f, d), Seq("Folder", "Document"))(
            classTag, Folder.extractorFolder, classTag, Document.extractorDocument, classTag
        ) ^^ "multiExtractorContainer"

    implicit lazy val multiExtractorStyleSelector: MultiExtractor[Seq[StyleSelector]] =
        multiExtractor2[StyleSelector, (Style, StyleMap), Style, StyleMap]((s, m) => (s, m), Seq("Style", "StyleMap"))(
            classTag, Style.extractorStyle, classTag, StyleMap.extractorStyleMap, classTag
        ) ^^ "multiExtractorStyleSelector"

//    implicit lazy val multiExtractorSubStyle: MultiExtractor[Seq[SubStyle]] =
//        multiExtractor3[SubStyle, (ColorStyle, BalloonStyle, ListStyle), ColorStyle, BalloonStyle, ListStyle]((p1,p2,p3) => (p1,p2,p3), Seq("ColorStyle","BalloonStyle", "ListStyle"))(
//            classTag, ColorStyle.extractorColorStyle, classTag, BalloonStyle.extractorBalloonStyle, classTag, ListStyle.extractorListStyle, classTag
//        ) ^^ "multiExtractorSubStyle"

    implicit lazy val multiExtractorColorStyle: MultiExtractor[Seq[ColorStyle]] =
        multiExtractor6[ColorStyle, (BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle), BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, LabelStyle](
            (p1, p2, p3, p4, p5, p6) => (p1, p2, p3, p4, p5, p6), Seq("BalloonStyle", "ListStyle", "PolyStyle", "LineStyle", "IconStyle", "LabelStyle")
        )(
            classTag, BalloonStyle.extractorBalloonStyle, classTag, ListStyle.extractorListStyle, classTag, PolyStyle.extractorPolyStyle, classTag, LineStyle.extractorLineStyle, classTag, IconStyle.extractorIconStyle, classTag, LabelStyle.extractorLabelStyle, classTag
        ) ^^ "multiExtractorColorStyle"

    implicit lazy val multiExtractorStyleMap: MultiExtractor[Seq[StyleMap]] = multiExtractorBase[StyleMap](StyleMap.extractorStyleMap) ^^ "multiExtractorStyleMap"
//     lazy val multiExtractorFolder: MultiExtractor[Seq[Folder]] = multiExtractorBase[Folder](Folder.extractorFolder) ^^ "multiExtractorFolder"
    implicit lazy val multiExtractorPair: MultiExtractor[Seq[Pair]] = multiExtractorBase[Pair](Pair.extractorPair) ^^ "multiExtractorPair"
    implicit lazy val multiExtractorCoordinates: MultiExtractor[Seq[Coordinates]] = multiExtractorBase[Coordinates](Coordinates.extractorCoordinates) ^^ "multiExtractorCoordinates"
//     lazy val multiExtractorPoint: MultiExtractor[Seq[Point]] = multiExtractorBase[Point](Point.extractorPoint) ^^ "multiExtractorPoint"
//     lazy val multiExtractorLineString: MultiExtractor[Seq[LineString]] = multiExtractorBase[LineString](LineString.extractorLineString) ^^ "multiExtractorLineString"
//     lazy val multiExtractorPlacemark: MultiExtractor[Seq[Placemark]] = multiExtractorBase[Placemark](Placemark.extractorPlacemark) ^^ "multiExtractorPlacemark"
//     lazy val multiExtractorDocument: MultiExtractor[Seq[Document]] = multiExtractorBase[Document](Document.extractorDocument) ^^ "multiExtractorDocument"
    implicit lazy val multiExtractorKml: MultiExtractor[Seq[KML]] = multiExtractorBase[KML](KML.extractorKml) ^^ "multiExtractorKml"

    implicit lazy val extractMaybeOpen: Extractor[Option[Int]] = extractorOption[Int](intExtractor) ^^ "extractMaybeOpen"
    implicit lazy val extractMaybeHeading: Extractor[Option[Heading]] = extractorOption[Heading](Heading.extractorHeading) ^^ "extractMaybeHeading"
    implicit lazy val extractMaybeListItemType: Extractor[Option[ListItemType]] = extractorOption[ListItemType](ListItemType.extractorListItemType) ^^ "extractMaybeListItemType"
    implicit lazy val extractMaybeTextColor: Extractor[Option[TextColor]] = extractorOption[TextColor](TextColor.extractorTextColor) ^^ "extractMaybeTextColor"
    implicit lazy val extractMaybeItemIcon: Extractor[Option[ItemIcon]] = extractorOption[ItemIcon](ItemIcon.extractorItemIcon) ^^ "extractMaybeItemIcon"
    implicit lazy val extractorMaybeBgColor: Extractor[Option[BgColor]] = extractorOption[BgColor](BgColor.extractorBgColor) ^^ "extractorMaybeBgColor"
    implicit lazy val extractMaybeColor: Extractor[Option[Color]] = extractorOption[Color](Color.extractorColor) ^^ "extractMaybeColor"
    implicit lazy val extractMaybeColorMode: Extractor[Option[ColorMode]] = extractorOption[ColorMode](ColorMode.extractorColorMode) ^^ "extractMaybeColorMode"

    /**
     * It appears that we don't need these extractors.
     */
//    implicit lazy val extractMaybeIconStyle: Extractor[Option[IconStyle]] = extractorOption[IconStyle] ^^ "extractMaybeIconStyle"
//    implicit lazy val extractMaybeLabelStyle: Extractor[Option[LabelStyle]] = extractorOption[LabelStyle] ^^ "extractMaybeLabelStyle"
//    implicit lazy val extractMaybeBalloonStyle: Extractor[Option[BalloonStyle]] = extractorOption[BalloonStyle] ^^ "extractMaybeBalloonStyle"
//    implicit lazy val extractMaybeLineStyle: Extractor[Option[LineStyle]] = extractorOption[LineStyle] ^^ "extractMaybeLineStyle"

    /**
     * The following extractors do not need to be declared implicit.
     */
    lazy val extractorKD2FD: Extractor[KmlData => FeatureData] = extractorPartial41(FeatureData.apply)(extractorText, extractorOptionalText, extractorOptionalText, extractMaybeOpen, multiExtractorStyleSelector, classTag) ^^ "extractorKD2FD"
    lazy val extractorGD2Point: Extractor[GeometryData => Point] = extractorPartial01(Point.apply)(multiExtractorCoordinates, classTag) ^^ "extractorGD2Point"
    lazy val extractorFD2Placemark: Extractor[FeatureData => Placemark] = extractorPartial01(Placemark.apply)(multiExtractorGeometry, classTag) ^^ "extractorFD2Placemark"
    lazy val extractorSSD2ColorStyleData: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(ColorStyleData.apply)(extractMaybeColor, extractMaybeColorMode, classTag) ^^ "extractorSSD2ColorStyleData"
    lazy val extractorCSD2PolyStyle: Extractor[ColorStyleData => PolyStyle] = extractorPartial20(PolyStyle.apply)(Fill.extractorFill, Outline.extractorOutline, classTag) ^^ "extractorCSD2PolyStyle"
    lazy val extractorCSD2ListStyle: Extractor[ColorStyleData => ListStyle] = extractorPartial30(ListStyle.apply)(BgColor.extractorBgColor, extractMaybeListItemType, extractMaybeItemIcon, classTag) ^^ "extractorCSD2ListStyle"
    lazy val extractorCSP2IconStyle: Extractor[ColorStyleData => IconStyle] = extractorPartial40(IconStyle.apply)(Scale.extractorScale,Icon.extractorIcon,HotSpot.extractorHotspot, extractMaybeHeading, classTag) ^^ "extractorCSP2IconStyle"
    lazy val extractorCSD2BalloonStyle: Extractor[ColorStyleData => BalloonStyle] = extractorPartial40(BalloonStyle.apply)(extractorText, extractorMaybeBgColor, extractMaybeTextColor, DisplayMode.extractorDisplayMode, classTag) ^^ "extractorCSD2BalloonStyle"
    lazy val extractorCSD2LabelStyle: Extractor[ColorStyleData => LabelStyle] = extractorPartial10(LabelStyle.apply)(Scale.extractorScale, classTag) ^^ "extractorCSD2LabelStyle"
    lazy val extractorCSD2LineStyle: Extractor[ColorStyleData => LineStyle] = extractorPartial10(LineStyle.apply)(Width.extractorWidth, classTag) ^^ "extractorCSD2LineStyle"
    lazy val extractorSSD2Style: Extractor[StyleSelectorData => Style] = extractorPartial01(Style.apply)(multiExtractorColorStyle, classTag) ^^ "extractorSSD2Style"
     val extractorSSD2StyleMap: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(StyleMap.apply)(multiExtractorPair, classTag) ^^ "extractorSSD2StyleMap"
    lazy val extractorCD2Folder: Extractor[ContainerData => Folder] = extractorPartial01(Folder.apply)(multiExtractorFeature, classTag) ^^ "extractorCD2Folder"
    lazy val extractorCD2Document: Extractor[ContainerData => Document] = extractorPartial01(Document.apply)(multiExtractorFeature, classTag) ^^ "extractorCD2Document"

    val junk = Seq(multiExtractorStyleMap, multiExtractorKml)
//
//    lazy val junk1 = Seq(multiExtractorGeometry, multiExtractorFeature, multiExtractorContainer, multiExtractorStyleSelector, multiExtractorSubStyle, multiExtractorColorStyle, multiExtractorStyleMap, multiExtractorFolder, multiExtractorPair, multiExtractorCoordinates, multiExtractorPoint, multiExtractorLineString, multiExtractorPlacemark, multiExtractorDocument, multiExtractorKml)
//    lazy val junk2 = Seq(extractMaybeOpen, extractMaybeHeading, extractMaybeListItemType, extractMaybeTextColor, extractMaybeItemIcon, extractorMaybeBgColor, extractMaybeColor, extractMaybeColorMode, extractMaybeIconStyle, extractMaybeLabelStyle, extractMaybeBalloonStyle, extractMaybeLineStyle)
//    lazy val junk3 = Seq(extractorKD2GeometryData, extractorKD2FD, extractorGD2Point, extractorFD2Placemark, extractorKD2Scale, extractorKD2SubStyleData, extractorSSD2ColorStyleData, extractorCSD2PolyStyle, extractorCSD2ListStyle, extractorCSP2IconStyle, extractorCSD2BalloonStyle, extractorCSD2LabelStyle, extractorCSD2LineStyle, extractorKD2StyleSelectorData, extractorSSD2Style, extractorSSD2StyleMap, extractorFD2ContainerData, extractorCD2Folder, extractorCD2Document)
//    lazy val junk4 = Seq(KmlData.extractorKmlData, Scale.extractorScale, Feature.extractorFeature, FeatureData.extractorFeatureData, Placemark.extractorPlacemark, Container.extractorContainer,
//            ContainerData.extractorContainerData, Folder.extractorFolder, Document.extractorDocument, Geometry.extractorGeometry, GeometryData.extractorGeometryData, Point.extractorPoint, StyleSelector.extractorStyleSelector,
//        StyleSelectorData.extractorStyleSelectorData, SubStyleData.extractorSubStyleData, ColorStyle.extractorColorStyle, ColorStyleData.extractorColorStyleData,
//        BalloonStyle.extractorBalloonStyle, ListStyle.extractorListStyle, LineStyle.extractorLineStyle, PolyStyle.extractorPolyStyle, LabelStyle.extractorLabelStyle,
//        IconStyle.extractorIconStyle, Fill.extractorFill, Outline.extractorOutline, Heading.extractorHeading, BgColor.extractorBgColor, TextColor.extractorTextColor,
//        DisplayMode.extractorDisplayMode, ListItemType.extractorListItemType, State.extractorState, ItemIcon.extractorItemIcon, Icon.extractorIcon, HotSpot.extractorHotspot, Color.extractorColor,
//        ColorMode.extractorColorMode, Width.extractorWidth, KML.extractorKml, Style.extractorStyle, Pair.extractorPair, StyleMap.extractorStyleMap, Tessellate.extractorTessellate,
//        LineString.extractorLineString, Coordinates.extractorCoordinates)

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

    implicit lazy val rendererOptionColor: Renderable[Option[Color]] = optionRenderer[Color] ^+ "rendererOptionColor"
    implicit lazy val rendererOptionBgColor: Renderable[Option[BgColor]] = optionRenderer[BgColor] ^+ "rendererOptionBgColor"
    implicit lazy val rendererOptionTextColor: Renderable[Option[TextColor]] = optionRenderer[TextColor] ^+ "rendererOptionTextColor"
    implicit lazy val rendererOptionColorMode: Renderable[Option[ColorMode]] = optionRenderer[ColorMode] ^+ "rendererOptionColorMode"
    implicit lazy val rendererOptionHeading: Renderable[Option[Heading]] = optionRenderer[Heading] ^+ "rendererOptionHeading"
    implicit lazy val rendererOptionListItemType: Renderable[Option[ListItemType]] = optionRenderer[ListItemType] ^+ "rendererOptionListItemType"
    implicit lazy val rendererOptionState: Renderable[Option[State]] = optionRenderer[State] ^+ "rendererOptionState"
    implicit lazy val rendererOptionItemIcon: Renderable[Option[ItemIcon]] = optionRenderer[ItemIcon] ^+ "rendererOptionItemIcon"
    implicit lazy val rendererOptionLineStyle: Renderable[Option[LineStyle]] = optionRenderer[LineStyle] ^+ "rendererOptionLineStyle"
    implicit lazy val rendererOptionLabelStyle: Renderable[Option[LabelStyle]] = optionRenderer[LabelStyle] ^+ "rendererOptionLabelStyle"
    implicit lazy val rendererOptionBalloonStyle: Renderable[Option[BalloonStyle]] = optionRenderer[BalloonStyle] ^+ "rendererOptionBalloonStyle"
    implicit lazy val rendererOptionIconStyle: Renderable[Option[IconStyle]] = optionRenderer[IconStyle] ^+ "rendererOptionIconStyle"
    implicit lazy val rendererColorStyles: Renderable[Seq[ColorStyle]] = sequenceRenderer[ColorStyle] ^+ "rendererColorStyles"
    implicit lazy val rendererSequencePair: Renderable[Seq[Pair]] = sequenceRenderer[Pair] ^+ "rendererSequencePair"
    implicit lazy val rendererCoordinates1: Renderable[Seq[Coordinate]] = sequenceRendererFormatted[Coordinate](FormatCoordinate) ^+ "rendererCoordinates1"
    // TODO refactor the sequenceRendererFormatted method so that its parameter is a Format=>Format function.
    implicit lazy val rendererCoordinates_s: Renderable[Seq[Coordinates]] = sequenceRendererFormatted[Coordinates](FormatXML) ^+ "rendererCoordinates_s"
    implicit lazy val rendererLineStrings: Renderable[Seq[LineString]] = sequenceRenderer[LineString] ^+ "rendererLineStrings"
    implicit lazy val rendererPoints: Renderable[Seq[Point]] = sequenceRenderer[Point] ^+ "rendererPoints"
    implicit lazy val rendererGeometrys: Renderable[Seq[Geometry]] = sequenceRenderer[Geometry] ^+ "rendererGeometrys"
    implicit lazy val rendererPlacemarks: Renderable[Seq[Placemark]] = sequenceRenderer[Placemark] ^+ "rendererPlacemarks"
    implicit lazy val rendererFeatures: Renderable[Seq[Feature]] = sequenceRenderer[Feature] ^+ "rendererFeatures"
    implicit lazy val rendererStyles: Renderable[Seq[Style]] = sequenceRenderer[Style] ^+ "rendererStyles"
    implicit lazy val rendererStyleMaps: Renderable[Seq[StyleMap]] = sequenceRenderer[StyleMap] ^+ "rendererStyleMaps"
    implicit lazy val rendererFolders: Renderable[Seq[Folder]] = sequenceRenderer[Folder] ^+ "rendererFolders"
    implicit lazy val rendererDocuments: Renderable[Seq[Document]] = sequenceRenderer[Document] ^+ "rendererDocuments"
    implicit lazy val rendererStyleSelectors: Renderable[Seq[StyleSelector]] = sequenceRenderer[StyleSelector] ^+ "rendererStyleSelectors"
    implicit lazy val rendererKml_Binding: Renderable[KML_Binding] = Renderable {
        (t: KML_Binding, format: Format, stateR: StateR) => doRenderKML_Binding(t, format, stateR)
    } ^^ "rendererKml_Binding"

    private def doRenderKML_Binding(t: KML_Binding, format: Format, stateR: StateR): Try[String] =
        TryUsing(stateR.addAttribute(s"""${t.binding}"""))(rs => implicitly[Renderable[KML]].render(t.kml, format, rs))
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
