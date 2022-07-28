package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.xml.{Extractor, Extractors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}
import scala.xml.{Elem, Node}

class KmlSpec extends AnyFlatSpec with should.Matchers {

  object KmlExtractors extends Extractors {
    implicit val extractorCoordinates: Extractor[Coordinates] = new Extractor[Coordinates] {
      def extract(node: Node): Try[Coordinates] = {
        val text = node.text
        Success(Coordinates.parse(text))
      }
    }
    implicit val extractorCoordinatesSequence: Extractor[Seq[Coordinates]] = extractorSequence[Coordinates]("coordinates")

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

  it should "extract Coordinate" in {
    val xml: Elem = <xml>
      <coordinates>
        -71.06992,42.49424,0
        -71.07018,42.49512,0
      </coordinates>
    </xml>
    val z: Try[Seq[Coordinates]] = extractorCoordinatesSequence.extract(xml)
    println(s"z=$z")
  }
}
