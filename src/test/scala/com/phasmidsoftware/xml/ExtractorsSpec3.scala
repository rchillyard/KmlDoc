package com.phasmidsoftware.xml

import com.phasmidsoftware.xml.Extractors.stringMultiExtractor
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Try
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
     * @param _id an optional identifier.
     */
    case class KmlData(_id: String)

    object KmlData extends Extractors {
        import Extractor.stringExtractor
        implicit val extractor: Extractor[KmlData] = extractor10(apply)
    }

    class geometrys extends KmlObject

    case class GeometryData(kmlData: KmlData)

    object GeometryData {
        val applyFunction: KmlData => GeometryData = new GeometryData(_)
    }

    case class Point(x: Double, y: Double)(val geometryData: GeometryData) extends geometrys

    /**
     * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
     */
    class StyleSelector() extends KmlObject

    object StyleSelector

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

    case class Simple($: String)(val superObject: Base)

    object MyExtractors extends Extractors {
        implicit val extractorBase: Extractor[Base] = extractor10(Base)
        implicit val extractorSimple: Extractor[Simple] = extractorPartial[Base, Simple](extractorPartial10(Simple.apply))
        implicit val extractorKPP2GeometryData: Extractor[KmlData => GeometryData] = extractorPartial0[KmlData, GeometryData](GeometryData.applyFunction)
        implicit val extractorGeometryData: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorKPP2GeometryData)
        implicit val extractorGD2Point: Extractor[GeometryData => Point] = extractorPartial20(Point.apply)
        implicit val extractorPoint: Extractor[Point] = extractorPartial[GeometryData, Point](extractorGD2Point)
        implicit val extractorMultiPoint: MultiExtractor[Seq[Point]] = multiExtractorBase[Point]
        // TODO this should also support Style. However, that causes a stack overflow
        implicit val extractorStyleSelector: Extractor[StyleSelector] = extractorAlt[StyleSelector, Style, StyleMap]//Extractor.none[StyleSelector].|[StyleMap]()
    }

    behavior of "Extractors"

    it should "extract simple" in {
        import MyExtractors._
        val xml: Elem = <simple id="2">Robin</simple>
        val extracted: Try[Simple] = Extractor.extract[Simple](xml)
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
        l.colorStyleData.subStyleData.kmlData._id shouldBe "2"
    }
    it should "extract Point" in {
        import MyExtractors._
        val xml: Elem = <xml>
            <Point id="2">
                <x>1.0</x> <y>2.0</y>
            </Point>
        </xml>
        val py: Try[Seq[Point]] = implicitly[MultiExtractor[Seq[Point]]].extract(xml / "Point")
        py.isSuccess shouldBe true
        val p = py.get.head
        println(p)
        p.x shouldBe 1.0
        p.y shouldBe 2.0
        p.geometryData.kmlData._id shouldBe "2"
    }
}
