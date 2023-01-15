package com.phasmidsoftware.xml

import com.phasmidsoftware.core.XmlException
import com.phasmidsoftware.xml.Extractor.{extract, extractMulti}
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

        import Extractors._

        implicit val extractor: Extractor[KmlData] = extractor10(apply)
    }

    class Geometry extends KmlObject

    object Geometry extends Extractors {
        implicit lazy val multiExtractorGeometry: MultiExtractor[Seq[Geometry]] =
            multiExtractor1[Geometry, Point, Point](identity, Seq("Point"))
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
        //             extractorPartial20(apply)
//        implicit lazy val extractorPoint: Extractor[Point] = extractorPartial[GeometryData, Point](extractorPartial)
implicit val extractor: Extractor[Point] = extractorPartial[GeometryData, Point](extractorPartial)
        implicit val extractorMulti: MultiExtractor[Seq[Point]] = multiExtractorBase[Point]
    }


    case class Coordinates(coordinates: Seq[Coordinate])

    object Coordinates extends Extractors {
        implicit lazy val extractor: Extractor[Coordinates] = Extractor(node => Success(Coordinates.parse(node.text)))
        implicit lazy val extractorMulti: MultiExtractor[Seq[Coordinates]] = multiExtractorBase[Coordinates]

        def parse(w: String): Coordinates =
            Coordinates((for (line <- Source.fromString(w).getLines(); if line.trim.nonEmpty) yield Coordinate(line)).toSeq)
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
        implicit lazy val extractor: Extractor[Scale] = extractorPartial[KmlData, Scale](partialExtractor)

//        def nemo(x: Double): Scale = new Scale(x)(KmlData.nemo)
    }

    Extractor.translations += "coordinates" -> Seq("coordinates")

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

}
