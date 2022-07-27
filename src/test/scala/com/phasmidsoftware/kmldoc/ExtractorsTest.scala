package com.phasmidsoftware.kmldoc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Success
import scala.xml.{Elem, XML}

class ExtractorsTest extends AnyFlatSpec with should.Matchers {

  case class Simple(x: Int)

  object MyExtractors extends Extractors {

    import Extractors._

    implicit val extractSimple: Extractor[Simple] = extractor1(Simple)
  }


  behavior of "ExtractorsTest"

  it should "extractorSeq" in {

  }

  it should "extractor1" in {
    val xml: Elem = <xml id="1"></xml>
    val id = MyExtractors.extractSimple.extract((xml \ "@id").head)
    id shouldBe Success(Simple(1))
  }

  it should "extractSeq" in {

  }

}
