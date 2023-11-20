package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.core.TryUsing
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLAsFormat
import com.phasmidsoftware.render.{FormatText, FormatXML, Renderer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Try

class KMLCompanionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "KMLCompanionSpec"

  it should "renderFeature" in {

  }

  it should "extractFeatures" in {

  }

  it should "renderFeatures" in {

  }

  it should "renderKMLAsFormat Text" in {
    val z: IO[String] = renderKMLAsFormat("sample.kml", FormatText(0)) map (_.mkString("\n"))
    val result = z.unsafeRunSync()
    println(result)
    val prefix = "KML{[Document{"
    result.substring(0, prefix.length) shouldBe prefix
  }

  it should "renderKMLAsFormat KML" in {
    val z: IO[String] = renderKMLAsFormat("sample.kml", FormatXML(0)) map (_.mkString("\n"))
    val result = z.unsafeRunSync()
    val prefix =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<kml xmlns="http://www.opengis.net/kml/2.2">
        |  <Document>""".stripMargin
    result.substring(0, prefix.length) shouldBe prefix
  }

  it should "loadKML" in {
    val z: IO[Seq[KML]] = KMLCompanion.loadKML(scala.util.Success("src/main/resources/com/phasmidsoftware/kmldoc/sample.kml"))
    val y: Seq[KML] = z.unsafeRunSync()
    import KML.renderer
    val r: Try[String] = TryUsing(KML.stateR)(sr => {
      val kml: KML = y.head

      Renderer.render(kml, FormatXML(0), sr)
    })
    println(r.get)
  }

}
