package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.xml.{Extractor, Extractors}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}
import scala.xml.{Elem, Node}

class KmlSpec extends AnyFlatSpec with should.Matchers {

  object KmlExtractors extends Extractors {

    import Extractors._

    implicit val extractorCoordinates: Extractor[Coordinates] = (node: Node) => Success(Coordinates.parse(node.text))
    implicit val extractorCoordinatesSequence: Extractor[Seq[Coordinates]] = extractorSequence[Coordinates]("coordinates")
    implicit val extractorLineString: Extractor[LineString] = extractor2(LineString)
    implicit val extractorLineStringSequence: Extractor[Seq[LineString]] =
      extractorSequence[LineString]("LineString")
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
          -71.063036,42.505942,0
          -71.062199,42.506393,0
          -71.061373,42.506797,0
          -71.060514,42.507168,0
          -71.05963,42.50749,0
          -71.058744,42.507793,0
          -71.057918,42.508023,0
          -71.057081,42.508244,0
          -71.05618,42.50847,0
          -71.05124,42.50961,0
          -71.0479834,42.5103672,0
          -71.044797,42.511099,0
          -71.042415,42.511613,0
          -71.040623,42.512032,0
          -71.0387796,42.5124453,0
          -71.0379372,42.5126125,0
          -71.0373734,42.5127148,0
          -71.0367666,42.5128092,0
          -71.034797,42.513053,0
          -71.027534,42.51393,0
          -71.020335,42.514871,0
          -71.0161776,42.5154163,0
          -71.012138,42.515899,0
          -71.009864,42.516145,0
          -71.00848,42.516279,0
          -71.007063,42.51635,0
          -71.001517,42.516516,0
          -70.9998214,42.5166188,0
          -70.9972246,42.5168956,0
          -70.9961464,42.5169393,0
          -70.9950253,42.5169198,0
          -70.9934395,42.5168691,0
          -70.9913511,42.5168108,0
          -70.988399,42.5168949,0
          -70.9787091,42.5171527,0
          -70.971165,42.517315,0
          -70.964684,42.517449,0
          -70.960747,42.517552,0
          -70.960039,42.517592,0
          -70.9592559,42.5176868,0
          -70.95829,42.517821,0
          -70.956413,42.518169,0
          -70.954449,42.518564,0
          -70.946327,42.520225,0
          -70.943012,42.520873,0
          -70.941253,42.521229,0
          -70.9401574,42.52148,0
          -70.9395935,42.5216214,0
          -70.9391422,42.5218299,0
          -70.9387339,42.5220187,0
          -70.9382665,42.5221086,0
          -70.9376275,42.5221016,0
          -70.9373267,42.5221001,0
          -70.937026,42.522154,0
          -70.9349023,42.5226188,0
          -70.9327606,42.5230921,0
          -70.9324166,42.5231742,0
          -70.9320618,42.5232741,0
          -70.9317326,42.5233846,0
          -70.9314035,42.5235089,0
          -70.9308132,42.5237619,0
          -70.9302573,42.5240613,0
          -70.9297751,42.5243196,0
          -70.9295033,42.5244637,0
          -70.9292556,42.5246178,0
          -70.9290368,42.5247581,0
          -70.9288234,42.5249438,0
          -70.9284487,42.5252919,0
          -70.928067,42.525649,0
          -70.9276835,42.5259992,0
          -70.9274804,42.5261317,0
          -70.9272746,42.5262424,0
          -70.927016,42.526353,0
          -70.9267707,42.5264256,0
          -70.926436,42.5264831,0
          -70.9262606,42.5264913,0
          -70.926065,42.5264708,0
          -70.9257614,42.5264001,0
          -70.9254345,42.5262817,0
        </coordinates>
      </LineString>
    </xml>
    val value1 = extractorLineStringSequence.extract(xml)
    println(value1)
    value1 match {
      case Success(ls) =>
        ls.size shouldBe 1
        val cs: Seq[Coordinates] = ls.head.coordinates
        cs.size shouldBe 1
        cs.head.coordinates.size shouldBe 96
      case Failure(x) => fail(x)
    }
  }
}
