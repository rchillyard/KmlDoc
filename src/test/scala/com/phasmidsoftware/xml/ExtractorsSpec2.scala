package com.phasmidsoftware.xml

import com.phasmidsoftware.render._
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Success, Try, Using}
import scala.xml.Elem

class ExtractorsSpec2 extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

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

    class Geometry extends KmlObject

    case class GeometryData(kmlData: KmlData)

    object GeometryData {
        val applyFunction: KmlData => GeometryData = new GeometryData(_)
    }

    case class Point(x: Double, y: Double)(val geometryData: GeometryData) extends Geometry

    /**
     * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
     */
    class StyleSelector() extends KmlObject

    case class StyleSelectorData(kmlData: KmlData)

    /**
     * Trait to allow Style and StyleMap to be alternatives in the sequence member of Document.
     */
    class SubStyle() extends KmlObject

    case class SubStyleData(kmlData: KmlData)

    object SubStyleData {
        val applyFunction: KmlData => SubStyleData = new SubStyleData(_)
    }

    class ColorStyle() extends SubStyle

    case class ColorStyleData(color: Long, maybeColorMode: Option[String])(val subStyleData: SubStyleData)

    case class LineStyle(width: Double)(val colorStyleData: ColorStyleData) extends ColorStyle

    case class StyleMap(Pairs: Seq[String])(val styleSelectorData: StyleSelectorData) extends StyleSelector

    case class Base(_id: Int)

    case class Simple($: String)(val superObject: Base)

    import Extractors._

    object MyExtractors extends Extractors {
        implicit val extractorBase: Extractor[Base] = extractor10(Base)
        implicit val extractorSimple: Extractor[Simple] = extractorPartial[Base, Simple](extractorPartial10(Simple.apply))
        implicit val extractorKmlData: Extractor[KmlData] = extractor10(KmlData)
        implicit val extractorKPP2GeometryData: Extractor[KmlData => GeometryData] = extractorPartial0[KmlData, GeometryData](GeometryData.applyFunction)
        implicit val extractorGeometryData: Extractor[GeometryData] = extractorPartial[KmlData, GeometryData](extractorKPP2GeometryData)
        implicit val extractorGD2Point: Extractor[GeometryData => Point] = extractorPartial20(Point.apply)
        implicit val extractorPoint: Extractor[Point] = extractorPartial[GeometryData, Point](extractorGD2Point)
        implicit val extractorMultiPoint: MultiExtractor[Seq[Point]] = multiExtractor[Point]
        implicit val extractorKPP2SubStyleData: Extractor[KmlData => SubStyleData] = extractorPartial0[KmlData, SubStyleData](SubStyleData.applyFunction)
        implicit val extractorSubStyleData: Extractor[SubStyleData] = extractorPartial[KmlData, SubStyleData](extractorKPP2SubStyleData)
        implicit val extractorSSP2ColorStyleData: Extractor[SubStyleData => ColorStyleData] = extractorPartial20(ColorStyleData.apply)
        implicit val extractorColorStyleData: Extractor[ColorStyleData] = extractorPartial[SubStyleData, ColorStyleData](extractorSSP2ColorStyleData)
        implicit val extractorCSP2LineStyle: Extractor[ColorStyleData => LineStyle] = extractorPartial10(LineStyle.apply)
        implicit val extractorLineStyle: Extractor[LineStyle] = extractorPartial[ColorStyleData, LineStyle](extractorCSP2LineStyle)
        implicit val extractorColorStyle: Extractor[ColorStyle] = Extractor.none[ColorStyle].|[LineStyle]()
        implicit val extractorMultiColorStyle: MultiExtractor[Seq[ColorStyle]] = multiExtractor[ColorStyle]
        implicit val extractorStyleSelectorData: Extractor[StyleSelectorData] = extractor10(StyleSelectorData.apply)
        implicit val extractorBT2: Extractor[StyleSelectorData => StyleMap] = extractorPartial01(StyleMap.apply)
        implicit val extractorStyleMap: Extractor[StyleMap] = extractorPartial[StyleSelectorData, StyleMap](extractorBT2)
        implicit val extractorStyleSelector: Extractor[StyleSelector] = Extractor.none[StyleSelector].|[StyleMap]()
        implicit val extractorMultiStyleSelector: MultiExtractor[Seq[StyleSelector]] = multiExtractor[StyleSelector]
    }

    import Renderers._

    object MyRenderers extends Renderers {
        implicit val renderableBase: Renderable[Base] = renderer1[Int, Base](Base.apply)
        implicit val renderableSimple: Renderable[Simple] = renderer1Super(Simple.apply)(_.superObject)
        implicit val rendererKmlData: Renderable[KmlData] = renderer1(KmlData.apply)
        implicit val rendererGeometryData: Renderable[GeometryData] = renderer0Super(GeometryData.apply)(_.kmlData)
        implicit val renderablePoint: Renderable[Point] = renderer2Super(Point.apply)(_.geometryData)
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
        import MyExtractors._
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

    behavior of "Renderers"

    it should "render Base" in {
        import MyRenderers._
        val base = Base(2)
        val wy = Using(StateR())(sr => implicitly[Renderable[Base]].render(base, FormatXML(0), sr))
        wy shouldBe Success("""<Base id="2" ></Base>""")
    }

    it should "render Simple" in {
        import MyRenderers._
        val simple = Simple("Robin")(Base(2))
        println(s"element 0: ${simple.productElement(0)}")
        val renderer: Renderable[Simple] = implicitly[Renderable[Simple]]
        val wy = Using(StateR())(sr => renderer.render(simple, FormatXML(0), sr))
        wy shouldBe Success("""<Simple id="2">Robin</Simple>""")
    }

    it should "render Point" in {
        import MyRenderers._
        val point = Point(1.0, 2.0)(GeometryData(KmlData("42")))
        println(point)
        val renderer: Renderable[Point] = implicitly[Renderable[Point]]
        val wy = Using(StateR())(sr => renderer.render(point, FormatXML(0), sr))
        // TODO this isn't a good rendering of Point. x and y should be tagged.
        wy shouldBe Success("""<Point id="42">x="1.0"y="2.0"</Point>""")
    }
}
