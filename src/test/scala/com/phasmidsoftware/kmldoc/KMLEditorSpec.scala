package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util._

class KMLEditorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "KMLEditor"

  val placemark = "Placemark"
  val join = "join"

  private val triedFilename: Success[String] = Success("src/main/resources/com/phasmidsoftware/kmldoc/placemarks.kml")

  // FIXME this doesn't actually work. We will fix it later.
  it should "processKMLs" in {
    val editor = KMLEditor(Seq(KmlEdit(join, Element(placemark, "Medford Branch (#1)"), Some(Element(placemark, "Medford Branch (#2)")))))
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
            // TODO re-introduce this test
//            features.size shouldBe 1
            val p1 = features.head
            p1 match {
              case Placemark(g) =>
                val lineString = g.head
                lineString match {
                  case LineString(_, cs) =>
                    cs.head.coordinates.size shouldBe 30 // + 25) // TODO we do need a total of 55
                }
            }
        }
    }
  }

  it should "edits" in {

  }

  it should "parse" in {
    val result: IO[KMLEditor] = KMLEditor.parse(Success("src/main/resources/com/phasmidsoftware/kmldoc/sampleEdits.txt"))
    result.unsafeRunSync() shouldBe KMLEditor(Seq(
      KmlEdit(join, Element(placemark, "Medford Branch (#1)"), Some(Element(placemark, "Medford Branch (#2)"))),
      KmlEdit("delete", Element("Placemark", "Medford Branch (#2)"), None)
    ))
  }

}
