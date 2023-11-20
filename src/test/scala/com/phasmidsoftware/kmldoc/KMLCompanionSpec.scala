package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLAsFormat
import com.phasmidsoftware.render.{FormatText, FormatXML}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class KMLCompanionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "KMLCompanionSpec"

  it should "renderFeature" in {

  }

  it should "extractFeatures" in {

  }

  it should "renderFeatures" in {

  }

  it should "renderKMLToPrintStream" in {

  }

  it should "renderKMLAsFormat Text" in {
    val z: IO[String] = renderKMLAsFormat("sample.kml", FormatText(0)) map (_.mkString("\n"))
    val result = z.unsafeRunSync()
    result.startsWith("Document{") shouldBe true
  }

  it should "renderKMLAsFormat KML" in {
    val z: IO[String] = renderKMLAsFormat("sample.kml", FormatXML(0)) map (_.mkString("\n"))
    val result = z.unsafeRunSync()
    result.startsWith("<Document>") shouldBe true
  }

  it should "loadKML" in {

  }

}
