package com.phasmidsoftware.kmldoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}
import scala.xml.Elem

class ExtractorsTest extends AnyFlatSpec with should.Matchers {

  case class Simple(x: Int)

  case class Empty()

  object MyExtractors extends Extractors {

    import Extractors._

    implicit val extractSimple: Extractor[Simple] = extractor1(Simple)
  }


  behavior of "ExtractorsTest"

  it should "extractorSeq" in {
    implicit val ee: Extractor[Empty] = MyExtractors.extractor0[Empty](_ => Empty())
    val xml: Elem = <xml>
      <doc></doc> <doc></doc>
    </xml>
    val extractedSeq: Try[Seq[Empty]] = MyExtractors.extractorSeq[Empty]("doc").extract(xml)
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

  it should "extractor0" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Empty](_ => Empty()).extract(xml)
    extracted shouldBe Success(Empty())
  }

  it should "extractor1" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = MyExtractors.extractSimple.extract((xml \ "@id").head)
    extracted shouldBe Success(Simple(1))
  }

}
