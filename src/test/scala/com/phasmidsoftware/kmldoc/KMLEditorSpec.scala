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

  it should "process" in {
    val result: IO[KMLEditor] = KMLEditor.parse(Success("src/main/resources/com/phasmidsoftware/kmldoc/sampleEdits.txt"))
    result.unsafeRunSync() shouldBe KMLEditor(Seq(KmlEdit(join, Element(placemark, "Medford Branch (#1)"), Some(Element(placemark, "Medford Branch (#2)")))))
  }

  it should "edits" in {

  }

  it should "parse" in {

  }

}
