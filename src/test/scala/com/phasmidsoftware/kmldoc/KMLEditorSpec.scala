package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util._

class KMLEditorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "KMLEditor"

  val placemark = "Placemark"
  private val triedFilename: Success[String] = Success("src/main/resources/com/phasmidsoftware/kmldoc/placemarks.kml")

  it should "processKMLs" in {
    val editor = KMLEditor(Seq(KmlEdit(KmlEdit.JOIN, 2, Element(placemark, "Salem & Lowell RR (#1)"), Some(Element(placemark, "Salem & Lowell RR (#2)"))), KmlEdit(KmlEdit.DELETE, 1, Element(placemark, "Salem & Lowell RR (#2)"), None)))
    val ksi: IO[Seq[KML]] = for {
      ks <- KMLCompanion.loadKML(triedFilename)
      ks2 = editor.processKMLs(ks)
    } yield ks2
    val result = ksi.unsafeRunSync()
    val feature: Feature = result.head.features.head
    feature match {
      case Document(fs) =>
        val folder = fs.head
        folder match {
          case Folder(features) =>
            features.size shouldBe 1
            val p1 = features.head
            p1 match {
              case Placemark(g) =>
                val lineString = g.head
                lineString match {
                  case LineString(_, cs) =>
                    cs.head.coordinates.size shouldBe 163
                    // TODO check the ordering of the Coordinate values.
                }
            }
        }
    }
  }

  it should "edits" in {

  }

  it should "parse" in {
    val result: IO[KMLEditor] = KMLEditor.parse(Success("src/main/resources/com/phasmidsoftware/kmldoc/sampleEdits.txt"))
    val editor = result.unsafeRunSync()
    editor.edits.take(2) shouldBe Seq(
      KmlEdit(KmlEdit.JOIN, 2, Element(placemark, "Salem & Lowell RR (#1)"), Some(Element(placemark, "Salem & Lowell RR (#2)"))),
      KmlEdit(KmlEdit.DELETE, 1, Element(placemark, "Salem & Lowell RR (#2)"), None)
    )
  }

}
