package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.effect.IO.fromTry
import com.phasmidsoftware.core.FP.tryNotNull
import com.phasmidsoftware.core._
import com.phasmidsoftware.kmldoc.HasFeatures.editHasFeaturesToOption
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLToPrintStream
import com.phasmidsoftware.kmldoc.KmlEdit.{JOIN, JOINX, editFeatures}
import com.phasmidsoftware.kmldoc.KmlRenderers.sequenceRendererFormatted
import com.phasmidsoftware.kmldoc.Mergeable.{mergeOptions, mergeOptionsBiased, mergeSequence, mergeStringsDelimited}
import com.phasmidsoftware.render._
import com.phasmidsoftware.xml.Extractor.{extractMulti, intExtractor}
import com.phasmidsoftware.xml.MultiExtractorBase.{NonNegative, Positive}
import com.phasmidsoftware.xml._
import java.io.PrintStream
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
 * NOTE: there's some argument for having KmlObject extend scala.xml.Node but it's not compelling.
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
case class KmlData(__id: Option[String]) extends Mergeable[KmlData] {

  /**
   * Method to merge KmlData objects.
   *
   * @param k a KmlData object.
   * @return the merged value of KmlData.
   */
  def merge(k: KmlData, mergeName: Boolean = true): Option[KmlData] = Some(KmlData(mergeStringsDelimited(__id, k.__id)("#")))
}

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
trait Feature extends KmlObject {

  /**
   * Method to edit this Feature, given a sequence of sibling Features such that we may be able to combine features.
   *
   * @param e  the edit to apply.
   * @param fs the siblings (which actually do include this itself).
   * @return an optional Feature.
   */
  def editToOption(e: KmlEdit, fs: Seq[Feature]): Option[Feature] =
    this match {
      case p: Placemark => p.editToOptionOption(e, fs).getOrElse(Some(this)) // CONSIDER returning None
      case d: Document => editHasFeaturesToOption(d)(e)(_fs => d.copy(features = _fs)(d.containerData))
      case x: Folder => editHasFeaturesToOption(x)(e)(_fs => x.copy(features = _fs)(x.containerData))
      case _ => Some(this) // Container // CONSIDER returning None
    }

}

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
case class FeatureData(name: Text, maybeDescription: Option[Text], maybeStyleUrl: Option[Text], maybeOpen: Option[Open], maybeVisibility: Option[Visibility], StyleSelectors: Seq[StyleSelector], abstractView: Seq[AbstractView])(val kmlData: KmlData) extends Mergeable[FeatureData] with HasName {
  /**
   * Method to merge FeatureData objects.
   *
   * @param f a FeatureData object.
   * @return the merged value of FeatureData.
   */
  def merge(f: FeatureData, mergeName: Boolean = true): Option[FeatureData] = {
    // TODO warn if styles are not the same.
    for {
      n <- if (mergeName) name merge f.name else Some(name)
      d = mergeOptions(maybeDescription, f.maybeDescription)((t1, t2) => t1 merge t2)
      z <- kmlData merge f.kmlData
    } yield FeatureData(n, d, maybeStyleUrl, maybeOpen, maybeVisibility, StyleSelectors, abstractView)(z) // TODO: not all fields are properly merged
  }
}

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
 * Trait to define the property of owning features.
 */
trait HasFeatures {
  val features: Seq[Feature]
}

/**
 * Companion object HasFeatures provides utilities for editing objects that have a property of owning features.
 */
object HasFeatures {

  /**
   * Method to edit an object that has features.
   *
   * @param edit the edit to be applied.
   * @param t    the object to edit.
   * @param g    a function which takes a Seq[Feature] and creates an optional new copy of the T based on the provided Feature sequence.
   * @tparam T the type of <code>t</code>.
   * @return an optional copy of <code>t</code> with a (potentially) new set of features.
   */
  def editHasFeaturesToOption[T](t: T)(edit: KmlEdit)(g: Seq[Feature] => T): Option[T] = t match {
    case h: HasFeatures => Some(g(editFeatures(edit, h.features)))
    case _ => throw new Exception(s"editHasFeaturesToOption: parameter t does not extend HasFeatures: ${t.getClass}")
  }

}

/**
 * Trait representing an entity that has a name attribute.
 *
 * A `HasName` entity provides access to its name, defined as a `Text` type.
 */
trait HasName {
  /**
   * Retrieves the name as a `Text` value.
   *
   * @return the name represented as a `Text` instance.
   */
  def name: Text
}

/**
 * Trait Geometry: abstract subelement of KmlObject.
 * Geometry is the super-type of LineString, Polygon, LinearRing, and Point.
 *
 * See [[https://developers.google.com/kml/documentation/kmlreference#geometry Geometry]]
 */
trait Geometry extends KmlObject with Mergeable[Geometry] with Invertible[Geometry] {

  /**
   * Merge this mergeable object with <code>t</code>.
   *
   * @param t the object to be merged with this.
   * @return the merged value of T.
   */
  def merge(t: Geometry, mergeName: Boolean = true): Option[Geometry] = None

  /**
   * Inverts the current Geometry instance.
   * This operation typically reverses the orientation or nature of the Geometry,
   * depending on its specific subtype (e.g., LineString, Polygon, etc.).
   *
   * @return an Option containing the inverted Geometry if applicable; otherwise, None.
   */
  def invert: Option[Geometry] = None
}

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
case class GeometryData(maybeExtrude: Option[Extrude], maybeAltitudeMode: Option[AltitudeMode])(val kmlData: KmlData) extends Mergeable[GeometryData] {
  /**
   * Merges the current GeometryData instance with another GeometryData instance.
   *
   * This method combines the properties of the current instance and the given instance
   * using defined merge rules.
   * The merging process considers attributes such as extrude
   * and altitude mode, while also merging underlying KML data.
   *
   * @param g         the GeometryData instance to merge with.
   * @param mergeName a Boolean flag indicating whether to merge names (default is true).
   * @return an Option of GeometryData, representing the merged result, or None if merging fails.
   */
  def merge(g: GeometryData, mergeName: Boolean = true): Option[GeometryData] =
    for {
      k <- kmlData merge g.kmlData
    } yield GeometryData(mergeOptionsBiased(maybeExtrude, g.maybeExtrude), mergeOptionsBiased(maybeAltitudeMode, g.maybeAltitudeMode))(k)
}

/**
 * Companion object for the `GeometryData` case class.
 *
 * Provides utilities and functionality for working with `GeometryData` such as
 * extractors and renderers which are essential for converting to and from external formats,
 * as well as rendering it into desired representations.
 *
 * Extends `Extractors` and `Renderers` to leverage common extraction and rendering utilities.
 *
 * Functionality:
 * - Defines a private partial extractor, `extractorPartial`, for constructing `GeometryData` instances.
 * - Provides an implicit `Extractor` instance for `GeometryData` to enable its seamless extraction.
 * - Provides an implicit `Renderer` instance for `GeometryData` for rendering purposes, tagged as "rendererGeometryData".
 */
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

/**
 * Companion object for the AbstractViewData case class.
 * Provides extractors and renderers
 * for handling AbstractViewData objects.
 *
 * NOTE: not currently used.
 *
 * Extends the Extractors and Renderers traits to facilitate extraction and rendering of
 * AbstractViewData instances.
 * Contains implicit values for extracting and rendering
 * AbstractViewData, as well as a partial extractor for more specific use cases.
 */
object AbstractViewData extends Extractors with Renderers {
  private val applyFunction: KmlData => AbstractViewData = new AbstractViewData(_)
  val extractorPartial: Extractor[KmlData => AbstractViewData] = extractorPartial0[KmlData, AbstractViewData](applyFunction)
  implicit val extractor: Extractor[AbstractViewData] = extractorPartial[KmlData, AbstractViewData](extractorPartial)
  implicit val renderer: Renderer[AbstractViewData] = renderer0Super(apply)(_.kmlData) ^^ "rendererAbstractViewData"
}

/**
 * Represents a "LookAt" view element in a KML document, describing a position to look at from a specific viewpoint,
 * with additional details like orientation, range, and optional altitude.
 * See [[https://developers.google.com/kml/documentation/kmlreference#lookat LookAt]]
 *
 * LookAt defines the geographic position (longitude, latitude), optional altitude, heading, tilt, and range
 * parameters for setting up a view in a geographic visualization context such as Google Earth.
 *
 * @param longitude         the longitude of the LookAt position in degrees.
 * @param latitude          the latitude of the LookAt position in degrees.
 * @param maybeAltitude     an optional altitude value for the LookAt position.
 * @param heading           the direction (in degrees) from the observer to the LookAt point.
 * @param tilt              the tilt angle (in degrees) of the view relative to the surface.
 * @param range             the distance (in meters) from the observer to the LookAt point.
 * @param maybeAltitudeMode an optional altitude mode specifying how the altitude is interpreted.
 * @param abstractViewData  additional data inherited from AbstractView.
 */
case class LookAt(longitude: Longitude, latitude: Latitude, maybeAltitude: Option[Altitude], heading: Heading, tilt: Tilt, range: Range, maybeAltitudeMode: Option[AltitudeMode])(val abstractViewData: AbstractViewData) extends AbstractView

/**
 * Companion object for the LookAt class, providing implicit extractors, renderers, and utilities
 * for working with LookAt instances.
 * LookAt represents a viewpoint in a KML visualization,
 * describing location, orientation, and viewing distance.
 *
 * It extends the functionality of Extractors and Renderers to allow for convenient serialization,
 * deserialization, and optional handling of LookAt objects.
 *
 * - `extractorPartial`: Defines a partial extractor for the LookAt class.
 * - `extractor`: An implicit extractor for deserializing LookAt instances.
 * - `extractorOpt`: An implicit extractor for handling optional LookAt instances.
 * - `renderer`: An implicit renderer for serializing LookAt instances.
 * - `rendererOpt`: An implicit renderer for serializing optional LookAt instances.
 */
object LookAt extends Extractors with Renderers {

  val extractorPartial: Extractor[AbstractViewData => LookAt] = extractorPartial70(apply)
  implicit val extractor: Extractor[LookAt] = extractorPartial[AbstractViewData, LookAt](extractorPartial)
  implicit val extractorOpt: Extractor[Option[LookAt]] = extractorOption[LookAt]
  implicit val renderer: Renderer[LookAt] = renderer7Super(apply)(_.abstractViewData)
  implicit val rendererOpt: Renderer[Option[LookAt]] = optionRenderer[LookAt]
}

/**
 * Case class Camera represents a viewpoint from which a KML feature is viewed.
 * See [[https://developers.google.com/kml/documentation/kmlreference#camera Camera]]
 *
 * The Camera defines a viewpoint using geographic coordinates and orientation information,
 * and it is a concrete implementation of the `AbstractView` trait.
 *
 * @param long              the longitude of the Camera's location.
 * @param lat               the latitude of the Camera's location.
 * @param alt               the altitude of the Camera's location.
 * @param heading           the direction that the Camera is facing, in degrees (0 = North).
 * @param tilt              the vertical angle between the Camera's direction and the surface, in degrees.
 * @param roll              the rotation about the Z-axis (normal to the screen), in degrees.
 * @param maybeAltitudeMode an optional altitude mode specifying how the altitude is interpreted.
 * @param abstractViewData  additional data associated with the Camera's AbstractView.
 */
case class Camera(long: Longitude, lat: Latitude, alt: Altitude, heading: Heading, tilt: Tilt, roll: Roll, maybeAltitudeMode: Option[AltitudeMode])(val abstractViewData: AbstractViewData) extends AbstractView

/**
 * Companion object for the Camera case class, providing Extractor and Renderer implementations.
 *
 * This object includes functionality to extract and render Camera instances using the defined
 * extractorPartial, extractor, renderer, and rendererOpt members.
 *
 * - `extractorPartial`: Represents a partial extractor for creating Camera instances based
 * on abstract view data.
 * - `extractor`: Implicitly provides the complete extractor for Camera instances.
 * - `renderer`: Implicitly provides the renderer for Camera instances, facilitating
 * conversion to a different representation.
 * - `rendererOpt`: Implicitly provides the renderer for optional Camera instances.
 */
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

/**
 * Companion object for the case class Icon.
 *
 * Extends functionality with Extractors and Renderers to enable safe conversion
 * and rendering for the Icon case class.
 * The object defines implicit extractor
 * and renderer instances for handling the Icon type.
 * The extractor and renderer
 * are utilized for serializing and deserializing Icon objects.
 */
object Icon extends Extractors with Renderers {

  import Renderers._

  implicit val extractor: Extractor[Icon] = extractor10(apply) ^^ "extractorIcon"
  implicit val renderer: Renderer[Icon] = renderer1(apply) ^^ "rendererIcon"
}

/**
 * Scale element: subelement of Object in the Kml reference.
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

  /**
   * Creates a new instance of the Scale class using the specified value and the `KmlData.nemo` context.
   *
   * @param x the value of the scale, represented as a Double.
   * @return a new instance of the Scale class configured with the provided value and `KmlData.nemo`.
   */
  def nemo(x: Double): Scale = new Scale(x)(KmlData.nemo)
}

/**
 * Trait StyleSelector is a sub-trait of KmlObject.
 * It is extended by Style and StyleMap.
 * See [[https://developers.google.com/kml/documentation/kmlreference#styleselector StyleSelector]]
 */
trait StyleSelector extends KmlObject

/**
 * Companion object of StyleSelector
 * that provides extractors and renderers for the `StyleSelector` trait and its subtypes.
 *
 * The `StyleSelector` trait is a sub-trait of `KmlObject` and is extended by `Style` and `StyleMap`.
 * This object includes implicit definitions for extraction and rendering of `StyleSelector` instances
 * and sequences of `StyleSelector` instances.
 * These operations are used to transform KML `StyleSelector`
 * elements into their corresponding internal representations and vice versa.
 *
 * Included Implicit Definitions:
 * - `extractorSeq`: Extractor for sequences of `StyleSelector` instances.
 * Combines multiple elements,
 * such as `StyleMap` and `Style`, into a sequence.
 * - `renderer`: Renderer for an individual `StyleSelector` instance. Transforms a single `StyleSelector`
 * into a renderable form.
 * - `rendererSeq`: Renderer for sequences of `StyleSelector` instances. Transforms a list of
 * `StyleSelector` into a renderable form.
 */
object StyleSelector extends Extractors with Renderers {
  implicit val extractorSeq: MultiExtractor[Seq[StyleSelector]] =
    multiExtractor2[StyleSelector, (StyleMap, Style), StyleMap, Style]((s, m) => (s, m), Seq("StyleMap", "Style")) ^^ "multiExtractorStyleSelector"
  implicit val renderer: Renderer[StyleSelector] = rendererSuper2[StyleSelector, Style, StyleMap] ^^ "rendererStyleSelector"
  implicit val rendererSeq: Renderer[Seq[StyleSelector]] = sequenceRenderer[StyleSelector] ^^ "rendererStyleSelectors"
}

/**
 * Case class StyleSelectorData: properties of StyleSelector and its subclasses.
 * There are no properties specific to StyleSelector.
 * See [[https://developers.google.com/kml/documentation/kmlreference#styleselector StyleSelector]]
 *
 * @param kmlData the KmlData reference.
 */
case class StyleSelectorData(kmlData: KmlData)

/**
 * Object StyleSelectorData.
 *
 * A companion object for the `StyleSelectorData` case class, providing methods and implicits
 * for extracting and rendering `StyleSelectorData` instances.
 * It extends the `Extractors`
 * and `Renderers` traits to leverage their functionality for data extraction and
 * rendering operations.
 *
 * This object includes:
 * - An `Extractor` for converting KmlData into `StyleSelectorData`.
 * - A `Renderer` for rendering `StyleSelectorData` into its corresponding output representation.
 */
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
 * Companion object for the SubStyle trait, providing utilities for extracting and rendering
 * instances of SubStyle and sequences of SubStyle.
 *
 * This object includes implicit values for extractors and renderers, which support:
 * - Extracting SubStyle and its subtypes using a `MultiExtractor`.
 * - Rendering instances of SubStyle and sequences of SubStyle using `Renderer`.
 *
 * The extractors and renderers are specifically tailored for the properties and subtypes of SubStyle,
 * such as BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, and LabelStyle.
 *
 * Note: Consider invoking ColorStyle when utilizing this object.
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

/**
 * Represents a substyle data container, which encapsulates KML (Keyhole Markup Language) data.
 * This class wraps an instance of KmlData, allowing additional processing or organization of KML metadata.
 *
 * @param kmlData an instance of KmlData containing metadata for the substyle.
 */
case class SubStyleData(kmlData: KmlData)

/**
 * Companion object for the SubStyleData case class.
 *
 * Provides functionality for extracting and rendering SubStyleData instances.
 * Includes various utilities to work with SubStyleData in the context of KML processing,
 * such as extractors and renderers for the structured data representation.
 *
 * The object is built upon the Extractors and Renderers traits, which allow for functional composition
 * while interacting with KmlData.
 * It defines an implicit extractor for parsing SubStyleData,
 * and an implicit renderer for converting SubStyleData back into a format suitable for output or transmission.
 */
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
case class Placemark(Geometry: Seq[Geometry])(val featureData: FeatureData) extends Feature with HasName with Mergeable[Placemark] with Invertible[Placemark] {

  /**
   * Inverts the current Placemark instance by reversing the orientation or nature
   * of its underlying geometric structure, if applicable.
   * This is achieved by attempting to invert each associated Geometry.
   *
   * @return an Option containing a new Placemark with inverted Geometries,
   *         or None if inversion is not possible.
   */
  def invert: Option[Placemark] = {
    val gos: Seq[Option[Geometry]] = for (g <- Geometry) yield g.invert
    val gso: Option[Seq[Geometry]] = FP.sequence(gos)
    for (gs <- gso) yield Placemark(gs)(featureData)
  }

  /**
   * Merge this mergeable object with <code>t</code>.
   *
   * @param t the object to be merged with this.
   * @return the merged value of T.
   */
  def merge(t: Placemark, mergeName: Boolean = true): Option[Placemark] = {
    logger.info(s"joinPlacemarks: $name, ${t.name} with mergeName=$mergeName")
    val los: Seq[Option[Geometry]] = for (gp <- this.Geometry; gq <- t.Geometry) yield gp.merge(gq)
    val gs: Seq[Geometry] = los filter (_.isDefined) map (_.get)
    for (fd <- featureData.merge(t.featureData, mergeName)) yield Placemark(gs)(fd)
  }

  override def toString: String = s"Placemark: name=${name.$} with ${Geometry.size} geometries: $Geometry"

  def name: Text = featureData.name

  /**
   * Method to edit this Feature, given a sequence of sibling Features such that we may be able to combine features.
   *
   * @param e  the edit which may (or may not) apply to <code>this</code>.
   * @param fs a sequence of Features which are the children of <code>this</code>'s family (including <code>p</code> itself).
   * @return an optional Feature.
   */
  def editToOptionOption(e: KmlEdit, fs: Seq[Feature]): Option[Option[Feature]] = e.operands match {
    case 1 => editMatching1(e)
    case 2 => editMatchingPlacemark2(e, fs)
  }

  /**
   * Method to edit the given Placemark with zero additional Features.
   *
   * @param e the edit which may (or may not) apply to <code>this</code>.
   * @return an optional Feature.
   */
  private def editMatching1(e: KmlEdit) = (name, e) match {
    case (name, KmlEdit(KmlEdit.DELETE, _, Element(_, nameToMatch), None))
      if name.matches(nameToMatch) =>
      logger.info(s"delete: $nameToMatch")
      Some(None)
    case (_, KmlEdit(KmlEdit.DELETE, _, _, _)) =>
      Some(Some(this))
    case (_, KmlEdit(KmlEdit.INVERT, _, Element(_, nameToMatch), _))
      if name.matches(nameToMatch) =>
      Some(this.invert)
    case _ =>
      None
  }

  /**
   * Method to process the given Placemark with one additional Features.
   *
   * @param e  the edit which may (or may not) apply to <code>p</code>.
   * @param fs a sequence of Features which are the children of <code>p</code>'s family (including <code>p</code> itself).
   * @return an optional optional Feature.
   */
  private def editMatchingPlacemark2(e: KmlEdit, fs: Seq[Feature]) =
    e match {
      case KmlEdit(command@(JOIN | JOINX), _, Element("Placemark", nameToMatch1), Some(Element("Placemark", nameToMatch2)))
        if name.matches(nameToMatch1) =>
        Some(joinMatchedPlacemarks(fs, nameToMatch2, command == JOIN))
      case _ =>
        None
    }

  /**
   * Method to join two Placemarks together.
   *
   * @param fs          the potential features to be joined with <code>p</code>. These are the siblings of <code>p</code> itself.
   * @param nameToMatch the name of the feature to be joined, as defined by the edit.
   * @return an optional Feature which, if defined, is the new Placemark to be used instead of <code>p</code>.
   */
  private def joinMatchedPlacemarks(fs: Seq[Feature], nameToMatch: String, mergeName: Boolean) = {
    val zz = for (f <- fs if f != this) yield joinMatchingPlacemarks(nameToMatch, f, mergeName)
    for (z <- zz.find(_.isDefined); q <- z) yield q
  }

  /**
   * Attempts to join a matching `Placemark` associated with the provided `Feature` based on the specified name.
   * If the feature is a `Placemark` and its name matches the given string, it attempts to merge this `Placemark`
   * with the given one.
   * Otherwise, returns `None`.
   *
   * @param name      the name to match against the `Placemark`'s name.
   * @param feature   the `Feature` which may or may not be a `Placemark` to attempt merging.
   * @param mergeName a flag indicating whether to merge the names of the two `Placemark` objects during the merge operation.
   * @return an optional merged `Placemark` or `None` if the names do not match or the feature is not a `Placemark`.
   */
  private def joinMatchingPlacemarks(name: String, feature: Feature, mergeName: Boolean) = feature match {
    case q: Placemark if q.name.matches(name) => merge(q, mergeName)
    // FIXME Issue #20 can result in this Placemark being lost if name doesn't match q.name
    case _ => None
  }
}

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
 * Container is a subtype of Feature and a super-type of Folder, Document.
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
 * Properties of a Container (and therefore all its subtypes).
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

/**
 * Companion object for the case class Point.
 *
 * This object provides utilities for working with the Point type,
 * including extractors and renderers to handle serialization and parsing.
 *
 * Utilities:
 * - Provides an implicit extractor to parse Point objects from GeometryData.
 * - Provides an implicit renderer for serializing Point objects.
 * - Provides a renderer for sequences of Point objects.
 */
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
case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry {

  import Coordinates.empty

  /**
   * Merges this `LineString` instance with the given `Geometry` instance.
   *
   * This method attempts to merge the attributes and data of the current `LineString` instance
   * with the provided `Geometry` instance if it is of type `LineString`.
   * The merging process involves combining tessellations, geometry data, and coordinate sequences,
   * with each step subject to specific merge rules.
   *
   * @param g         the `Geometry` instance to merge with.
   * @param mergeName a Boolean indicating whether to merge names (default is true).
   * @return an `Option` containing the merged `LineString` instance if successful, or `None` otherwise.
   */
  override def merge(g: Geometry, mergeName: Boolean = true): Option[Geometry] = g match {
    case l@LineString(_, _) =>
      for {
        t <- tessellate merge l.tessellate
        g <- geometryData merge l.geometryData
        c <- mergeSequence(coordinates)(empty).headOption
        d <- mergeSequence(l.coordinates)(empty).headOption
        z <- c merge d
      } yield LineString(t, Seq(z))(g)
  }

  /**
   * Returns an inverted version of this `LineString`, where the order of the coordinates
   * in each sequence is reversed.
   *
   * @return an `Option` containing the inverted `LineString` instance if successful.
   */
  override def invert: Option[LineString] = Some(LineString(tessellate, for (c <- coordinates) yield c.reverse)(geometryData))

}

/**
 * Object `LineString` serves as a companion to the `LineString` case class.
 * It provides extraction and rendering capabilities for `LineString` objects.
 *
 * The object includes implicit values for extraction and rendering to facilitate
 * seamless operations such as serialization, deserialization, and transformation
 * of `LineString` instances.
 *
 * This object is designed for internal configuration, such as connecting the
 * `Extractor` and `Renderer` abstractions with the `LineString` case class.
 */
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

/**
 * Companion object for the `Polygon` case class.
 *
 * Provides extractors and renderers that are useful for working with the `Polygon` type.
 * Specifically, it allows for extracting `Polygon` instances from `GeometryData` and rendering
 * `Polygon` instances to their desired outputs.
 *
 * This object extends `Extractors` and `Renderers`, enabling integration with extraction
 * and rendering functionalities of the system.
 *
 * Members:
 *
 * - `extractorPartial`: A private partial extractor specifically for handling `Polygon` extraction logic.
 * - `extractor`: An implicit `Extractor[Polygon]` that utilizes `extractorPartial` for complete extraction functionality.
 * - `renderer`: An implicit `Renderer[Polygon]` that allows rendering a `Polygon` while leveraging its geometry data.
 */
object Polygon extends Extractors with Renderers {
  private val extractorPartial: Extractor[GeometryData => Polygon] = extractorPartial21(apply)
  implicit val extractor: Extractor[Polygon] = extractorPartial[GeometryData, Polygon](extractorPartial) ^^ "extractorPolygon"
  implicit val renderer: Renderer[Polygon] = renderer3Super(apply)(_.geometryData) ^^ "rendererPolygon"
}

/**
 * Represents the outer boundary of a polygon in KML format.
 * This case class holds a `LinearRing`, which defines the coordinates
 * that create the outer boundary.
 * See [[https://developers.google.com/kml/documentation/kmlreference#outerboundaryis outerBoundaryIs]]
 *
 * @param LinearRing the `LinearRing` instance that specifies the coordinates
 *                   of the outer boundary of a polygon.
 */
case class OuterBoundaryIs(LinearRing: LinearRing)

/**
 * Companion object for the `OuterBoundaryIs` case class.
 * This object provides utilities for extracting and rendering `OuterBoundaryIs` instances.
 *
 * The `extractor` is an implicit value utilized for extracting `OuterBoundaryIs` instances,
 * and it uses the `extractor10` combinator along with the `apply` method of `OuterBoundaryIs`.
 *
 * The `renderer` is an implicit value used for rendering `OuterBoundaryIs` instances into
 * their corresponding KML representations.
 * It employs the `renderer1` combinator with the `apply` method */
object OuterBoundaryIs extends Extractors with Renderers {
  implicit val extractor: Extractor[OuterBoundaryIs] = extractor10(apply) ^^ "extractorOuterBoundaryIs"
  implicit val renderer: Renderer[OuterBoundaryIs] = renderer1(apply) ^^ "rendererOuterBoundaryIs"
}

/**
 * This case class represents the inner boundary of a polygon in KML (Keyhole Markup Language) files.
 * The inner boundary is defined by a `LinearRing`,
 * which specifies a closed, linear path (a closed line with four or more positions).
 * See [[https://developers.google.com/kml/documentation/kmlreference#innerboundaryis InnerBoundaryIs]]
 *
 * @param LinearRing The `LinearRing` defining the closed linear path representing the inner boundary.
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

/**
 * Companion object for the LinearRing case class.
 *
 * This object provides extraction and rendering functionalities for LinearRing instances.
 * It defines an implicit extractor and renderer for working with LinearRing objects.
 * The extractorPartial method is used to create a partial extractor specific to the LinearRing type.
 * The extractor is responsible for extracting LinearRing instances using GeometryData.
 * The renderer is responsible for rendering LinearRing instances back into a specific representation.
 */
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

/**
 * Style object that provides utilities for extracting and rendering `Style` objects.
 * This object extends functionality provided by `Extractors` and `Renderers`.
 *
 * Extraction and rendering are achieved using partial extractors and renderers that align
 * with the handling of `StyleSelectorData` and `Style` instances.
 *
 * Members:
 * - `extractorPartial`: Defines a partial extractor for mapping `StyleSelectorData` to `Style`.
 * - `extractor`: An implicit full extractor for `Style` built from `extractorPartial`.
 * - `renderer`: An implicit full renderer for `Style` that uses its `styleSelectorData`.
 * - `rendererSeq`: An implicit renderer to handle sequences of `Style` objects.
 */
object Style extends Extractors with Renderers {
  val extractorPartial: Extractor[StyleSelectorData => Style] = extractorPartial01(apply) ^^ "extractorSSD2Style"
  implicit val extractor: Extractor[Style] = extractorPartial[StyleSelectorData, Style](extractorPartial) ^^ "extractorStyle"
  implicit val renderer: Renderer[Style] = renderer1Super(apply)(_.styleSelectorData) ^^ "rendererStyle"
  implicit val rendererSeq: Renderer[Seq[Style]] = sequenceRenderer[Style] ^^ "rendererStyles"
}

/**
 * Case class StyleMap represents a KML StyleMap element, which acts as a mapping of key-value pairs,
 * where keys ('normal' or 'highlight') are mapped to specific styles using style URLs.
 * See [[https://developers.google.com/kml/documentation/kmlreference#stylemap StyleMap]]
 *
 * StyleMap is a subtype of StyleSelector, and it extends its functionality by providing a mechanism
 * to dynamically switch between styles based on the user interaction.
 *
 * @constructor Creates an instance of StyleMap with the given sequence of Pairs and a StyleSelectorData instance.
 * @param Pairs             a sequence of key-value pairs, where each key is an instance of Key (e.g., "normal", "highlight")
 *                          and each value is an instance of StyleURL pointing to the respective style.
 * @param styleSelectorData an instance of StyleSelectorData that associates this StyleMap with additional KML metadata.
 */
case class StyleMap(Pairs: Seq[Pair])(val styleSelectorData: StyleSelectorData) extends StyleSelector

/**
 * The `StyleMap` object provides utility methods and implicit definitions for working with the `StyleMap` class.
 * It includes extractors and renderers that facilitate parsing and formatting `StyleMap` instances.
 *
 * Features:
 * - Partial and full extractors for converting between `StyleSelectorData` and `StyleMap`.
 * - Implicit renderers for serializing `StyleMap` instances and sequences of `StyleMap` objects.
 */
object StyleMap extends Extractors {

  import KmlRenderers._

  val extractorPartial: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(apply) ^^ "extractorSSD2StyleMap"
  implicit val extractor: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorPartial) ^^ "extractorStyleMap"
  implicit val renderer: Renderer[StyleMap] = renderer1Super(apply)(_.styleSelectorData) ^^ "rendererStyleMap"
  implicit val rendererSeq: Renderer[Seq[StyleMap]] = sequenceRenderer[StyleMap] ^^ "rendererStyleMaps"
}

/**
 * Case class BalloonStyle which extends ColorStyle.
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

/**
 * Object BalloonStyle provides extractors and renderers for the `BalloonStyle` case class.
 *
 * This object includes functionalities for extracting and rendering `BalloonStyle`, allowing conversions and
 * serializations between the case class and its representations in other formats.
 * It leverages the `Extractors`
 * and `Renderers` traits, and provides several implicit instances for convenience.
 *
 * It defines:
 *
 * - `extractorPartial`: A partial extractor for constructing `BalloonStyle` from `ColorStyleData`.
 * - `extractor`: An implicit extractor for `BalloonStyle` objects.
 * - `renderer`: An implicit renderer for `BalloonStyle` objects.
 * - `rendererOpt`: An implicit renderer for optional `BalloonStyle` instances (`Option[BalloonStyle]`).
 */
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

/**
 * Companion object for the `ListStyle` case class, providing extractors and renderers.
 *
 * This object includes functionality for extracting and rendering `ListStyle` instances,
 * utilizing partial extractors and renderers.
 * This allows seamless integration with
 * serialization or transformation frameworks.
 *
 * Members:
 * - `extractorPartial`: A partial extractor for converting `ColorStyleData` to `ListStyle`.
 * - `extractor`: An implicit extractor for `ListStyle`, providing the ability to transform data into a `ListStyle` instance.
 * - `renderer`: An implicit renderer for `ListStyle`, enabling transformation of `ListStyle` instances into a rendered format.
 */
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
 * Companion object for the ColorStyle trait.
 *
 * Provides extractors, renderers, and helper definitions for working with instances of ColorStyle
 * and related types including BalloonStyle, ListStyle, PolyStyle, LineStyle, IconStyle, and LabelStyle.
 *
 * Includes:
 * - A multiExtractor for extracting sequences of ColorStyle instances from various representations.
 * - A renderer for rendering ColorStyle instances.
 * - A renderer for rendering sequences of ColorStyle instances.
 *
 * This */
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

/**
 * Companion object for the `ColorStyleData` case class.
 *
 * Provides Extractors and Renderers required to efficiently process `ColorStyleData`.
 *
 * @define extractorPartial Defines a partial extractor for `ColorStyleData`
 *                          by converting `SubStyleData` into `ColorStyleData` with the help of the `apply` method.
 * @define extractor        An implicit full extractor for `ColorStyleData`, built atop the partial extractor to facilitate conversions.
 * @define renderer         An implicit renderer for `ColorStyleData`, mapping it back to its super representation.
 */
object ColorStyleData extends Extractors with Renderers {

  val extractorPartial: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(apply) ^^ "extractorSSD2ColorStyleData"
  implicit val extractor: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorPartial) ^^ "extractorColorStyleData"
  implicit val renderer: Renderer[ColorStyleData] = renderer2Super(apply)(x => x.subStyleData) ^^ "rendererColorStyleData"
}

/**
 * Case class Folder: subelement of Container.
 * See [[https://developers.google.com/kml/documentation/kmlreference#container Folder]].
 *
 * @param features      a sequence of Feature elements (where Feature is an abstract super-type).
 * @param containerData the ContainerData (auxiliary property).
 */
case class Folder(features: Seq[Feature])(val containerData: ContainerData) extends Container with HasFeatures with HasName {
  /**
   * Retrieves the name feature from the container data's feature data.
   *
   * @return The name represented as a `Text` object extracted from the container data.
   */
  def name: Text = containerData.featureData.name

  override def toString: String = s"Folder: name=${name.$} with ${features.size} features"
}

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
case class Document(features: Seq[Feature])(val containerData: ContainerData) extends Container with HasFeatures with HasName {
  def name: Text = containerData.featureData.name

  override def toString: String = s"Document: name=${name.$} with ${features.size} features"
}

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

/**
 * Companion object for the `LineStyle` case class, providing extractors and renderers.
 *
 * This object includes partial extractors, implicit extractors, and renderers for `LineStyle` instances.
 * It facilitates conversion to and from data structures in the context of `LineStyle`.
 *
 * Members:
 * - `extractorPartial`: Partial extractor for `ColorStyleData` */
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

/**
 * Companion object for PolyStyle which provides extractor and renderer utilities.
 * This object combines Extractors and Renderers traits to facilitate the transformation
 * and rendering of PolyStyle instances.
 *
 * Members:
 * - extractorPartial: Extractor for transforming ColorStyleData into PolyStyle.
 * - extractor: Implicit Extractor instance for PolyStyle.
 * - renderer: Implicit Renderer instance for PolyStyle.
 */
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

/**
 * An object that provides extraction and rendering utilities for the `IconStyle` case class.
 *
 * `IconStyle` represents styling information for icons, including color style data
 * and additional properties such as scale, icon details, hotspot, and heading.
 *
 * This object includes the following:
 *
 * - `extractorPartial`: A partial extractor that transforms `ColorStyleData` to `IconStyle`.
 * - `extractor`: An implicit extractor for extracting `IconStyle` instances.
 * - `renderer`: An implicit renderer for rendering `IconStyle` instances.
 * - `rendererOpt`: An implicit renderer for rendering optional `IconStyle` instances.
 */
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

/**
 * Companion object for the LabelStyle case class.
 *
 * This object provides functionality for extracting, rendering, and
 * managing LabelStyle instances.
 * It extends Extractors and Renderers
 * to leverage shared utilities for handling data conversion and manipulation.
 *
 * Members:
 * - `extractorPartial`: A partial extractor from ColorStyleData to LabelStyle.
 * - `extractor`: Fully defined extractor to convert inputs into LabelStyle instances.
 * - `renderer`: Renderer implementation for LabelStyle instances.
 * - `rendererOpt`: Renderer for optional LabelStyle instances.
 */
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
case class Tessellate($: CharSequence) extends Mergeable[Tessellate] {
  def merge(t: Tessellate, mergeName: Boolean = true): Option[Tessellate] = ($, t.$) match {
    case (a, b) if a == b => Some(Tessellate(a))
    case _ => None
  }
}

/**
 * Object Tessellate provides extractors and renderers for instances of the Tessellate case class.
 *
 * This object contains implicit definitions for converting Tessellate instances
 * to and from various representations using Extractor and Renderer utilities.
 * It facilitates parsing and generation of Tessellate representations in a structured manner.
 */
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

/**
 * The `Extrude` object serves as a companion to the `Extrude` case class.
 * It extends the `Extractors` and `Renderers` traits to provide both
 * extraction and rendering capabilities for the `Extrude` type.
 *
 * This object includes implicit definitions for various extractors and renderers,
 * making it suitable for transforming and handling operations with `Extrude`
 * instances and their optional counterparts.
 *
 * Implicit extractors:
 * - `extractor`: Provides extraction logic for `Extrude` using `extractor10`.
 * - `extractorOpt`: Provides optional extraction logic for `Extrude` using `extractorOption`.
 *
 * Implicit renderers:
 * - `renderer`: Supplies rendering capabilities for `Extrude` instances.
 * - `rendererOpt`: Supplies rendering capabilities for optional `Extrude` instances.
 */
object Extrude extends Extractors with Renderers {

  import Renderers._

  implicit val extractor: Extractor[Extrude] = extractor10(apply) ^^ "extractorExtrude"
  implicit val extractorOpt: Extractor[Option[Extrude]] = extractorOption[Extrude] ^^ "extractorOptionExtrude"
  implicit val renderer: Renderer[Extrude] = renderer1(apply)
  implicit val rendererOpt: Renderer[Option[Extrude]] = optionRenderer[Extrude]
}

/**
 * Represents an Open element, usually capturing a character sequence.
 *
 * @param $ the content represented as a CharSequence
 */
case class Open($: CharSequence)

/**
 * Companion object for the `Open` case class.
 * Provides extractor and renderer instances for working with the `Open` type and `Option[Open]`.
 *
 * Mixes in functionality from the `Extractors` and `Renderers` traits to enable operations such as
 * extraction and rendering for instances of the `Open` class.
 */
object Open extends Extractors with Renderers {

  import Renderers._

  implicit val extractor: Extractor[Open] = extractor10(apply)
  implicit val extractorOpt: Extractor[Option[Open]] = extractorOption[Open]
  implicit val renderer: Renderer[Open] = renderer1(apply)
  implicit val rendererOpt: Renderer[Option[Open]] = optionRenderer[Open]
}

/**
 * A case class representing visibility in KML documents.
 *
 * This case class encapsulates a `CharSequence` value that signifies visibility attributes in KML data.
 * Visibility settings often dictate whether a specific KML element should be displayed in a viewer.
 */
case class Visibility($: CharSequence)

/**
 * Companion object for the `Visibility` case class.
 *
 * This object provides implicits and utilities for serializing and deserializing `Visibility` instances.
 * It extends traits `Extractors` and `Renderers` to support extraction and rendering functionalities.
 *
 * Implicit extractors and renderers are provided for both `Visibility` and `Option[Visibility]`, enabling their usage
 * in data processing pipelines that require standardized extraction or rendering logic.
 */
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

/**
 * Represents a sequence of `Coordinate` objects and provides operations for merging, reversing, and computing
 * geometric properties such as gaps and directions.
 *
 * @constructor Creates an instance of `Coordinates` with the specified sequence of `Coordinate` objects.
 * @param coordinates A sequence of `Coordinate` objects representing this collection.
 */
case class Coordinates(coordinates: Seq[Coordinate]) extends Mergeable[Coordinates] {
  override def toString: String = {
    val sb = new StringBuilder("Coordinates{")
    sb.append(s"# coordinates: ${coordinates.size}, ")
    sb.append(s"from: ${coordinates.head}, ")
    sb.append(s"to: ${coordinates.last}")
    sb.append("}").toString()
  }

  private val result: Option[Cartesian] = for (last <- coordinates.lastOption; first <- coordinates.headOption; v <- first vector last) yield v
  private lazy val vector: Option[Cartesian] =
    result

  lazy val direction: Option[Double] = for (last <- coordinates.lastOption; first <- coordinates.headOption; d <- first distance last) yield d

  /**
   * Computes the gap (distance) between this Coordinates instance and another Coordinates instance, if possible.
   *
   * @param other the other Coordinates instance to compute the gap with
   * @return an Option containing the distance as a Double, or None if the distance cannot be calculated
   */
  def gap(other: Coordinates): Option[Double] = gapInternal(coordinates, other.coordinates)

  /**
   * Merges this `Coordinates` instance with another `Coordinates` instance.
   *
   * @param other     the other `Coordinates` instance to be merged with this instance.
   * @param mergeName a boolean indicating whether to merge the names of the two `Coordinates` instances. Defaults to true.
   * @return an `Option` containing the resulting merged `Coordinates`, or `None` if the merge cannot be performed.
   */
  def merge(other: Coordinates, mergeName: Boolean = true): Option[Coordinates] = {
    KMLCompanion.logger.info(s"merge $this with $other")
    // CONSIDER rejuvenating the following code to try to deal automatically with inversions.
//    val xo = vector
//    val yo: Option[Cartesian] = other.vector
//    val zo: Option[Double] = for (x <- xo; y <- yo) yield x dotProduct y
//    val q: Option[Coordinates] = for (z <- zo) yield if (z >= 0) other else other.reverse // no longer used
    mergeInternal(Some(other))
  }

  /**
   * Reverses the order of the coordinates contained in this `Coordinates` instance.
   *
   * @return a new `Coordinates` instance with the coordinates in reversed order.
   */
  def reverse: Coordinates = {
    KMLCompanion.logger.info(s"reversing $this")
    Coordinates(coordinates.reverse)
  }

  private def gapInternal(cs1: Seq[Coordinate], cs2: Seq[Coordinate]): Option[Double] =
    for {
      a <- cs1.lastOption
      b <- cs2.headOption
      q <- a.distance(b)
    } yield q

  private def mergeInternal(co: Option[Coordinates]): Option[Coordinates] =
    for {
      c <- co
      r <- gap(c)
      s <- c.gap(this)
    } yield
      Coordinates(if (r < s) coordinates ++ c.coordinates else c.coordinates ++ coordinates)
}

/**
 * Companion object for the `Coordinates` class.
 *
 * This object provides utilities for extracting, rendering, and manipulating instances of `Coordinates`.
 * It includes implicit extractors and renderers for `Coordinates` objects and sequences of `Coordinates`.
 * Additionally, it provides methods for parsing and handling operations related to `Coordinates` instances.
 */
object Coordinates extends Extractors with Renderers {
  implicit val extractor: Extractor[Coordinates] = Extractor(node => Success(Coordinates.parse(node.text))) ^^ "extractorCoordinates"
  implicit val extractorSeq: MultiExtractor[Seq[Coordinates]] = multiExtractorBase[Coordinates](Positive) ^^ "multiExtractorCoordinates"
  implicit val renderer: Renderer[Coordinates] = renderer1(apply) ^^ "rendererCoordinates"
  implicit val rendererSeq: Renderer[Seq[Coordinates]] = sequenceRendererFormatted[Coordinates](FormatXML(_)) ^^ "rendererCoordinates_s"

  def parse(w: String): Coordinates = Coordinates((for (line <- Source.fromString(w).getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)

  val empty: Coordinates = Coordinates(Nil)
}

/**
 * Case class Coordinate to represent a three-dimensional point.
 *
 * @param long longitude.
 * @param lat  latitude.
 * @param alt  altitude.
 */
case class Coordinate(long: String, lat: String, alt: String) {
  lazy val geometry: Option[Cartesian] = for (x <- long.toDoubleOption; y <- lat.toDoubleOption; z <- alt.toDoubleOption) yield Cartesian(x, y, z)

  def vector(c: Coordinate): Option[Cartesian] = for (a <- this.geometry; b <- c.geometry) yield a vector b

  def distance(c: Coordinate): Option[Double] = for (a <- this.geometry; b <- c.geometry) yield a distance b
}

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

/**
 * The `Fill` object provides extractor and renderer implementations for the `Fill` case class.
 * It offers mechanisms to extract `Fill` instances and render them as representations
 * suitable for processing or serialization.
 *
 * The object includes implicit extractors and renderers for both `Fill` and `Option[Fill]` types:
 * - Extractors parse and convert input representations into `Fill` instances.
 * - Renderers generate output representations from `Fill` instances.
 *
 * Operations within this object make use of the provided mechanisms in the `Extractors`
 * and `Renderers` traits for consistent and flexible processing pipelines.
 */
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

/**
 * The `Outline` object provides implicit extractors and renderers for the `Outline` type.
 *
 * It facilitates the extraction and rendering of `Outline` objects and their `Option` counterparts.
 * The object extends `Extractors` and `Renderers` to leverage shared behavior for parsing and formatting.
 *
 * Implicit members include:
 * - An `Extractor` for `Outline` objects, allowing the transformation of data into an `Outline` instance.
 * - An `Extractor` for `Option[Outline]`, supporting optional */
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

/**
 * Provides extractor and renderer instances for the Heading case class.
 *
 * This object facilitates the conversion of Heading instances to and from different formats,
 * enabling seamless interaction between data representations.
 * It includes implicit implementations
 * of extractors and renderers for both Heading and Option[Heading] types.
 *
 * The extractors and renderers adhere to a standardized naming convention:
 * - Extractor: Converts external data into Heading or Option[Heading] instances.
 * - Renderer: Converts Heading or Option[Heading] instances into a rendered form.
 */
object Heading extends Extractors with Renderers {

  import Renderers._

  implicit val extractor: Extractor[Heading] = extractor10(apply) ^^ "extractorHeading"
  implicit val extractorOpt: Extractor[Option[Heading]] = extractorOption[Heading] ^^ "extractMaybeHeading"
  implicit val renderer: Renderer[Heading] = renderer1(apply) ^^ "rendererHeading"
  implicit val rendererOpt: Renderer[Option[Heading]] = optionRenderer[Heading] ^^ "rendererOptionHeading"
}

/**
 * This case class represents a background color element with a single parameter.
 *
 * @param $ a CharSequence value that defines the background color.
 */
case class BgColor($: CharSequence)

/**
 * Companion object for the BgColor case class.
 * This object provides utilities
 * to extract and render BgColor instances.
 * It includes implicit definitions
 * for handling BgColor objects and optional BgColor instances using the Extractor
 * and Renderer type classes respectively.
 *
 * The Extractor instances are used to parse data into BgColor instances, while
 * the Renderer instances manage the conversion of BgColor objects back to their
 * desired output formats.
 *
 * Key functionalities:
 * - Provides an implicit extractor for BgColor to facilitate data extraction.
 * - Supplies an optional extractor to handle optional BgColor values.
 * - Defines a renderer for BgColor to support data transformation and output.
 * - Includes an optional renderer for handling optional BgColor values.
 */
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
case class KML(features: Seq[Feature]) extends HasFeatures {

  /**
   * Method to edit a KML object.
   *
   * @param e an edit to be applied to the given KML.
   * @return an optional KML that, if defined, is different from the input.
   */
  def edit(e: KmlEdit): Option[KML] = for (z <- editHasFeaturesToOption(this)(e)(fs => copy(features = fs))) yield z

}

/**
 * Companion object to class KML.
 *
 * NOTE that there are two different methods for rendering a KML object.
 * One is to use the KML_Binding mechanism.
 * The other is to invoke the method below: renderKml.
 */
object KML extends Extractors with Renderers {
  def init(): Unit = {
    TagProperties.addMustMatch("innerBoundaryIs")
  }

  implicit val extractor: Extractor[KML] = extractor01(apply) ^^ "extractorKml"
  implicit val extractorSeq: MultiExtractor[Seq[KML]] = multiExtractorBase[KML](NonNegative) ^^ "multiExtractorKml"
  implicit val renderer: Renderer[KML] = renderer1(apply) ^^ "rendererKml"

  /**
   * Method to render a KML using a fixed value for xmlns and a fixed prefix.
   *
   * @param kml    the KML object to be rendered.
   * @param format the format required.
   * @return a Try[String].
   */
  def renderKml(kml: KML, format: Format): Try[String] = {
    val r -> sR = format match {
      case _: FormatXML => rendererSpecial -> stateR
      case _ => implicitly[Renderer[KML]] -> StateR()
    }
    TryUsing(sR)(sr => r.render(kml, format, sr))
  }

  private val xmlnsAttribute = """xmlns="http://www.opengis.net/kml/2.2""""
  private val prefix = """<?xml version="1.0" encoding="UTF-8"?>"""

  // XXX Ensure that this remains a def
  def stateR = new StateR(Some("kml"), SmartBuffer(new StringBuilder(xmlnsAttribute)), false)

  private val rendererSpecial: Renderer[KML] = renderer1Special(apply, prefix + "\n") ^^ "rendererKml"
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

  import cats.implicits._

  val logger: Logger = LoggerFactory.getLogger(KML.getClass)

  def renderKMLToPrintStream(resourceName: String, format: Format, printStream: PrintStream = System.out): IO[Unit] = renderKMLAsFormat(resourceName, format) map (_.mkString("\n")) map printStream.println

  def renderKMLAsFormat(resourceName: String, format: Format): IO[Seq[String]] = for {
    fs <- KMLCompanion.loadKML(KML.getClass.getResource(resourceName))
    ws <- renderKMLs(fs, format)
  } yield ws

  /**
   * Note that this mechanism won't work if, as we wouldn't expect, there is more than one KML element.
   *
   * @param ks     the KML elements.
   * @param format the desired format.
   * @return a Seq[String] wrapped in IO.
   */
  def renderKMLs(ks: Seq[KML], format: Format): IO[Seq[String]] = (ks map (k => renderKML(k, format))).sequence

  private def renderKML(k: KML, format: Format): IO[String] = fromTry(KML.renderKml(k, format))

  def renderFeatures(fs: Seq[Feature], format: Format): IO[Seq[String]] = (fs map (f => renderFeature(f, format))).sequence

  private def renderFeature(f: Feature, format: Format): IO[String] = fromTry(TryUsing(StateR())(sr => implicitly[Renderer[Feature]].render(f, format, sr)))

  def loadKML(resource: URL): IO[Seq[KML]] = loadKML(
    for {
      u <- tryNotNull(resource)(s"resource $resource is null")
      p <- tryNotNull(u.getPath)(s"$resource yielded empty filename")
    } yield p
  )

  def loadKML(triedFilename: Try[String]): IO[Seq[KML]] =
    for {
      filename <- fromTry(triedFilename)
      elem <- IO(XML.loadFile(filename))
      fs <- fromTry(extractKML(elem))
    } yield fs


  def extractFeatures(xml: Elem): Try[Seq[Feature]] = {
    val kys: Seq[Try[Seq[Feature]]] = for (kml <- xml \\ "kml") yield Extractor.extractAll[Seq[Feature]](kml)
    kys.headOption match {
      case Some(ky) => ky
      case _ => Failure(new NoSuchElementException)
    }
  }

  private def extractKML(xml: Elem): Try[Seq[KML]] = extractMulti[Seq[KML]](xml)
}

object Test extends App {

  import cats.effect.unsafe.implicits.global

  private val ui = renderKMLToPrintStream("sample.kml", FormatText(0))

  ui.unsafeRunSync()
}

case class KmlException(str: String) extends Exception(str)
