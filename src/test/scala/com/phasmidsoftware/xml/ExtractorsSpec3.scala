package com.phasmidsoftware.xml

import com.phasmidsoftware.core.{Text, XmlException}
import com.phasmidsoftware.xml.Extractor.{extract, extractChildren, extractMulti}
import com.phasmidsoftware.xml.Extractors.stringMultiExtractor
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.Elem

class ExtractorsSpec3 extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

    /**
     * Super-type of all KML entities.
     * See https://developers.google.com/kml/documentation/kmlreference
     */
    class KmlObject

    /**
     * Properties of KMLObject
     *
     * @param __id an optional identifier.
     */
    case class KmlData(__id: Option[String])

    object KmlData extends Extractors {
        def nemo: KmlData = KmlData(None)

        import Extractors._

        implicit val extractor: Extractor[KmlData] = extractor10(apply)
    }

    class Geometry extends KmlObject

    object Geometry extends Extractors {
        implicit val multiExtractorGeometry: MultiExtractor[Seq[Geometry]] =
        // CONSIDER passing identity instead of (l,p) => (l,p)
            multiExtractor2[Geometry, (LineString, Point), LineString, Point]((l, p) => (l, p), Seq("LineString", "Point")) ^^ "multiExtractorGeometry"
    }

    case class GeometryData(kmlData: KmlData)

    object GeometryData extends Extractors {
        val applyFunction: KmlData => GeometryData = new GeometryData(_)
        val extractorPartial: Extractor[KmlData => GeometryData] = extractorPartial0[KmlData, GeometryData](applyFunction)
        implicit val extractor: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorPartial)
    }

    case class Point(coordinates: Seq[Coordinates])(val geometryData: GeometryData) extends Geometry


    //    case class Point(x: Double, y: Double)(val geometryData: GeometryData) extends Geometry

    object Point extends Extractors {
        val extractorPartial: Extractor[GeometryData => Point] = extractorPartial01(apply)
        implicit val extractor: Extractor[Point] = extractorPartial[GeometryData, Point](extractorPartial)
        implicit val extractorMulti: MultiExtractor[Seq[Point]] = multiExtractorBase[Point]
    }


    case class Tessellate($: String)

    object Tessellate extends Extractors {
        implicit val extractor: Extractor[Tessellate] = extractor10(apply)
    }

    case class LineString(tessellate: Tessellate, coordinates: Seq[Coordinates]) extends Geometry

    object LineString extends Extractors {
        implicit val extractor: Extractor[LineString] = extractor11(apply)
    }

    case class Coordinates(coordinates: Seq[Coordinate])

    object Coordinates extends Extractors {
        implicit val extractor: Extractor[Coordinates] = Extractor(node => Success(Coordinates.parse(node.text)))
        implicit val extractorMulti: MultiExtractor[Seq[Coordinates]] = multiExtractorBase[Coordinates]

        def parse(w: String): Coordinates = Coordinates((for (line <- Source.fromString(w).getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
    }


    case class Coordinate(long: String, lat: String, alt: String)

    object Coordinate {

        private val latLong: Regex = """\s*([\d\-\.]+),([\d\-\.]+),([\d\-\.]+)""".r

        def apply(w: String): Coordinate = w match {
            case latLong(long, lat, alt) => Coordinate(long, lat, alt)
            case _ => throw XmlException(s"bad coordinate string: $w")
        }
    }

    /**
     * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
     */
    class StyleSelector() extends KmlObject

    object StyleSelector extends Extractors {
        implicit val extractor: Extractor[StyleSelector] = extractorAlt[StyleSelector, Style, StyleMap]
        implicit val extractorMulti: MultiExtractor[Seq[StyleSelector]] =
            multiExtractor2[StyleSelector, (Style, StyleMap), Style, StyleMap]((s, m) => (s, m), Seq("Style", "StyleMap"))
    }

    case class StyleSelectorData(kmlData: KmlData)

    object StyleSelectorData extends Extractors {
        implicit val extractor: Extractor[StyleSelectorData] = extractor10(apply)
    }

    /**
     * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
     */
    class SubStyle() extends KmlObject

    case class SubStyleData(kmlData: KmlData)

    object SubStyleData extends Extractors {
        val applyFunction: KmlData => SubStyleData = new SubStyleData(_)
        lazy val extractorKPP2SubStyleData: Extractor[KmlData => SubStyleData] = extractorPartial0[KmlData, SubStyleData](applyFunction)
        implicit val extractor: Extractor[SubStyleData] = extractorPartial[KmlData, SubStyleData](extractorKPP2SubStyleData)
    }

    class ColorStyle() extends SubStyle

    object ColorStyle extends Extractors {
        implicit val extractor: Extractor[ColorStyle] = extractorSubtype[ColorStyle, LineStyle]
        implicit val multiExtractor: MultiExtractor[Seq[ColorStyle]] = multiExtractorBase[ColorStyle]
    }

    case class ColorStyleData(color: Long, maybeColorMode: Option[String])(val subStyleData: SubStyleData)

    object ColorStyleData extends Extractors {

        import Extractors._

        lazy val extractorSSP2ColorStyleData: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(apply)
        implicit val extractor: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorSSP2ColorStyleData)
    }

    case class LineStyle(width: Double)(val colorStyleData: ColorStyleData) extends ColorStyle

    object LineStyle extends Extractors {
        lazy val extractorCSP2LineStyle: Extractor[ColorStyleData => LineStyle] = extractorPartial10(apply)
        implicit val extractor: Extractor[LineStyle] = extractorPartial[ColorStyleData, LineStyle](extractorCSP2LineStyle)
    }

    case class StyleMap(Pairs: Seq[String])(val styleSelectorData: StyleSelectorData) extends StyleSelector

    object StyleMap extends Extractors {
        lazy val extractorBT2: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(apply)
        implicit val extractor: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorBT2)
    }

    case class Style(Styles: Seq[ColorStyle])(val styleSelectorData: StyleSelectorData) extends StyleSelector

    object Style extends Extractors {
        lazy val extractorSSD2Style: Extractor[StyleSelectorData => Style] = extractorPartial01(apply)
        implicit val extractor: Extractor[Style] = extractorPartial[StyleSelectorData, Style](extractorSSD2Style)
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
    object Feature extends Extractors {
        // NOTE we don't currently use extractorFeature.
        // CONSIDER should we use it instead of the mechanism in, for example, multiExtractor2.
        //    implicit lazy  val extractorFeature: Extractor[Feature] = extractorAlt[Feature,Container,Placemark](Container.extractorContainer,Placemark.extractorPlacemark)
        //    val applyFunction: Unit => Feature = _ => new Feature() // CONSIDER do we need this?

        // NOTE it works to make this val and have extractorCD2Folder, etc. as lazy val.
        implicit val multiExtractorFeature: MultiExtractor[Seq[Feature]] =
            multiExtractor1[Feature, Placemark, Placemark](identity, Seq("Placemark"))
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
    object FeatureData extends Extractors {

        import Extractors._

        lazy val extractorPartial: Extractor[KmlData => FeatureData] = extractorPartial41(apply)
        implicit val extractorFeatureData: Extractor[FeatureData] = extractorPartial[KmlData, FeatureData](extractorPartial)
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
    object Placemark extends Extractors {
        lazy val extractorPartial: Extractor[FeatureData => Placemark] = extractorPartial01(apply)

        import FeatureData.extractorFeatureData

        implicit val extractor: Extractor[Placemark] = extractorPartial[FeatureData, Placemark](extractorPartial)
    }


    case class Base(_id: Int)

    object Base extends Extractors {
        implicit val extractor: Extractor[Base] = extractor10(apply)
    }

    case class Simple($: String)(val superObject: Base)

    object Simple extends Extractors {
        implicit val extractor: Extractor[Simple] = extractorPartial[Base, Simple](extractorPartial10(apply))
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
    object Scale extends Extractors {

        val partialExtractor: Extractor[KmlData => Scale] = extractorPartial10(apply)
        implicit val extractor: Extractor[Scale] = extractorPartial[KmlData, Scale](partialExtractor)

//        def nemo(x: Double): Scale = new Scale(x)(KmlData.nemo)
    }

    Extractor.translations += "coordinates" -> Seq("coordinates")
    Extractor.translations += "Feature" -> Seq("Placemark", "Folder", "Document")
    Extractor.translations += "Geometry" -> Seq("LineString", "Point")
    Extractor.translations += "StyleSelector" -> Seq("Style", "StyleMap")

    behavior of "Extractors"

    it should "extract simple" in {
        val xml: Elem = <simple id="2">Robin</simple>
        val extracted: Try[Simple] = extract[Simple](xml)
        extracted.isSuccess shouldBe true
        extracted.get.$ shouldBe "Robin"
        extracted.get.superObject._id shouldBe 2
    }

    it should "extract LineStyle" in {
        val xml: Elem = <linestyle id="2">
            <width>1.0</width> <color>42</color>
        </linestyle>
        val ly: Try[LineStyle] = Extractor.extract[LineStyle](xml)
        println(ly)
        ly.isSuccess shouldBe true
        val l = ly.get
        l.width shouldBe 1.0
        l.colorStyleData.color shouldBe 42
        l.colorStyleData.subStyleData.kmlData.__id shouldBe Some("2")
    }
    it should "extract Point" in {
        val xml: Elem = <xml>
            <Point id="my point">
                <coordinates>
                    -71.097293,42.478238,0
                </coordinates>
            </Point>
        </xml>
        val py: Try[Seq[Point]] = extractMulti[Seq[Point]](xml / "Point")
        py.isSuccess shouldBe true
        val p = py.get.head
        py.get.head match {
            case Point(cs)
            =>
                cs.size shouldBe 1
                cs.head match {
                    case Coordinates(xs) =>
                        xs.size shouldBe 1
                        xs.head match {
                            case Coordinate("-71.097293", "42.478238", "0") =>
                            case c => fail(s"coordinate: $c")
                        }
                }

        }
        p.geometryData.kmlData.__id shouldBe Some("my point")
    }

    it should "parse Scale with id" in {
        val xml: Elem = <scale id="Hello">2.0</scale>
        val triedScale = extract[Scale](xml)
        triedScale.isSuccess shouldBe true
        triedScale.get.$ shouldBe 2.0
    }

    it should "parse Scale without id" in {
        val xml: Elem = <scale>2.0</scale>
        val triedScale = extract[Scale](xml)
        triedScale.isSuccess shouldBe true
        triedScale.get.$ shouldBe 2.0
    }

    it should "extract Point as geometry" in {
        val xml: Elem = <xml>
            <Point id="my point">
                <coordinates>
                    -71.097293,42.478238,0
                </coordinates>
            </Point>
        </xml>
        extractMulti[Seq[Geometry]](xml / "_") match {
            case Success(gs) =>
                gs.size shouldBe 1
                gs.head match {
                    case Point(cs) =>
                        cs.size shouldBe 1
                        cs.head.coordinates.size shouldBe 1
                }
            //                val wy = TryUsing(StateR())(sr => KmlRenderers.rendererGeometrys.render(gs, FormatXML(0), sr))
            //                wy.isSuccess shouldBe true
            //                println(wy.get)
            //                // TODO fix the rendering of Point: the following line is how it really SHOULD be:
            //                //        wy.get shouldBe "<Point>\n  <coordinates>\n    -71.097293, 42.478238, 0\n    </coordinates>\n  \n  </Point>"
            //                wy.get shouldBe "\n<Point id=\"my point\">\n    <coordinates>\n      -71.097293, 42.478238, 0\n      </coordinates>\n    \n    </Point>\n\n".format().stripMargin
            case Failure(x) => fail(x)
        }
    }

    it should "extract Placemark" in {
        val xml: Elem = <xml>
            <Placemark>
                <name>Wakefield Branch of Eastern RR</name>
                <description>RDK55. Also known as the South Reading Branch. Wakefield (S. Reading) Jct. to Peabody.</description>
                <styleUrl>#line-006600-5000</styleUrl>
                <LineString>
                    <tessellate>1</tessellate>
                    <coordinates>
                        -71.06992,42.49424,0
                        -71.07018,42.49512,0
                        -71.07021,42.49549,0
                        -71.07008,42.49648,0
                        -71.069849,42.497415,0
                        -71.06954,42.49833,0
                        -70.9257614,42.5264001,0
                        -70.9254345,42.5262817,0
                    </coordinates>
                </LineString>
            </Placemark>
        </xml>
        extractChildren[Seq[Feature]]("Feature")(xml) match {
//        extractAll[Seq[Feature]](xml) match {
            case Success(ps) =>
                ps.size shouldBe 1
                val feature: Feature = ps.head
                feature match {
                    case placemark: Placemark =>
                        val featureData: FeatureData = placemark.featureData
                        placemark.featureData.name shouldBe Text("Wakefield Branch of Eastern RR")
                        val geometry: Seq[Geometry] = placemark.Geometry
                        geometry.size shouldBe 1
                        geometry.head match {
                            case LineString(Tessellate("1"), coordinates) =>
                                coordinates.size shouldBe 1
                                val coordinate = coordinates.head
                                coordinate.coordinates.size shouldBe 8
                            case _ => fail("first Geometry is not a LineString")

                        }
                        featureData match {
                            case FeatureData(Text("Wakefield Branch of Eastern RR"), maybeDescription, _, _, Nil) =>
                                println(s"maybeDescription: $maybeDescription")
                            case _ => println(s"$featureData did not match the expected result")
                        }
                    //                        val wy = TryUsing(StateR())(sr => Renderable.render[Placemark](placemark, FormatXML(0), sr))
                    //                        wy.isSuccess shouldBe true
                    //                        trimWhiteSpace(wy.get) shouldBe trimWhiteSpace("<Placemark ><name>Wakefield Branch of Eastern RR</name><description>RDK55. Also known as the South Reading Branch. Wakefield (S. Reading) Jct. to Peabody.</description>" +
                    //                            "<styleUrl>#line-006600-5000</styleUrl>\n    " +
                    //                            "      \n      \n      " +
                    //                            "<LineString>" +
                    //                            "<tessellate>1</tessellate>\n      <coordinates>\n        -71.06992, 42.49424, 0\n        -71.07018, 42.49512, 0\n        -71.07021, 42.49549, 0\n        -71.07008, 42.49648, 0\n        -71.069849, 42.497415, 0\n        -71.06954, 42.49833, 0\n        -70.9257614, 42.5264001, 0\n        -70.9254345, 42.5262817, 0\n        </coordinates>\n      \n      " +
                    //                            "</LineString>" +
                    //                            "\n    \n    \n  \n  \n  </Placemark>")
                }
            case Failure(x) => fail(x)
        }
    }

    //    it should "extract Placemark" in {
    //        val coordinates1 = Coordinates(Seq(Coordinate("0", "-72", "0")))
    //        val point: Point = Point(Seq(coordinates1))(GeometryData(KmlData.nemo))
    //        val featureData: FeatureData = FeatureData(Text("Hello"), None, None, None, Nil)(KmlData.nemo)
    //        val placemark = Placemark(Seq(point))(featureData)
    ////        val wy = TryUsing(StateR())(sr => Renderable.render[Placemark](placemark, FormatXML(0), sr))
    ////        wy shouldBe Success("<Placemark ><name>Hello</name>\n      \n      \n      \n    <Point >\n        <coordinates>\n          -72, 0, 0\n          </coordinates>\n        \n        </Point>\n    \n    </Placemark>".stripMargin)
    //    }

}
