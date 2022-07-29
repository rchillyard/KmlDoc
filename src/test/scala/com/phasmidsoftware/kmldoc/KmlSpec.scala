package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.xml.{Extractor, Extractors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}
import scala.xml.{Elem, Node}

class KmlSpec extends AnyFlatSpec with should.Matchers {

  object KmlExtractors extends Extractors {

    import Extractors._

    implicit val extractorCoordinates: Extractor[Coordinates] =
      (node: Node) => Success(Coordinates.parse(node.text))
    implicit val extractorCoordinatesSequence: Extractor[Seq[Coordinates]] =
      extractorSequence[Coordinates]("coordinates")
    implicit val extractorLineString: Extractor[LineString] =
      extractor2(LineString)
    implicit val extractorLineStringSequence: Extractor[Seq[LineString]] =
      extractorSequence[LineString]("LineString")
    implicit val extractorPlacemark: Extractor[Placemark] =
      extractor4(Placemark)
    implicit val extractorPlacemarkSequence: Extractor[Seq[Placemark]] =
      extractorSequence[Placemark]("Placemark")
  }

  behavior of "Coordinate"

  it should "parse Coordinate pair" in {
    Coordinate("-71.06992,42.49424,0") shouldBe Coordinate("42.49424", "-71.06992")
  }
  behavior of "Coordinates"

  it should "parse Coordinates" in {
    val coordinates = Coordinates.parse(
      """-71.06992,42.49424,0
        |-71.07018,42.49512,0
        |""".stripMargin)
    coordinates.coordinates.size shouldBe 2
  }

  import KmlExtractors._

  it should "extract Coordinates" in {
    val xml: Elem = <xml>
      <coordinates>
        -71.06992,42.49424,0
        -71.07018,42.49512,0
      </coordinates>
    </xml>
    extractorCoordinatesSequence.extract(xml) match {
      case Success(cs) =>
        cs.size shouldBe 1
        cs.head.coordinates.size shouldBe 2
      case Failure(x) => fail(x)
    }
  }

  it should "extract LineString" in {
    val xml: Elem = <xml>
      <LineString>
        <tessellate>1</tessellate>
        <coordinates>
          -71.06992,42.49424,0
          -71.07018,42.49512,0
          -71.07021,42.49549,0
          -71.07008,42.49648,0
          -71.069849,42.497415,0
          -71.06954,42.49833,0
          -71.069173,42.49933,0
          -71.06879,42.50028,0
          -71.068121,42.501386,0
          -71.067713,42.501964,0
          -71.067327,42.502462,0
          -71.06634,42.503459,0
          -71.065825,42.503933,0
          -71.0653,42.504384,0
          -71.064742,42.504819,0
          -71.064205,42.505207,0
          -71.063637,42.505594,0
          -70.9254345,42.5262817,0
        </coordinates>
      </LineString>
    </xml>
    extractorLineStringSequence.extract(xml) match {
      case Success(ls) =>
        ls.size shouldBe 1
        val lineString = ls.head
        lineString.tessellate shouldBe "1"
        val cs: Seq[Coordinates] = lineString.coordinates
        cs.size shouldBe 1
        cs.head.coordinates.size shouldBe 18
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
    extractorPlacemarkSequence.extract(xml) match {
      case Success(ps) =>
        ps.size shouldBe 1
        val placemark: Placemark = ps.head
        placemark.name shouldBe "Wakefield Branch of Eastern RR"
        placemark.description shouldBe "RDK55. Also known as the South Reading Branch. Wakefield (S. Reading) Jct. to Peabody."
        val ls: Seq[LineString] = placemark.LineStrings
        ls.size shouldBe 1
        val lineString: LineString = ls.head
        val coordinates = lineString.coordinates
        coordinates.size shouldBe 1
        val coordinate = coordinates.head
        coordinate.coordinates.size shouldBe 8
      case Failure(x) => fail(x)
    }
  }
}

