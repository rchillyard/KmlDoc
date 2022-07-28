package com.phasmidsoftware.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}
import scala.xml.Elem

class ExtractorsTest extends AnyFlatSpec with should.Matchers {

  case class Simple(x: Int)

  case class Empty()

  case class Document(empties: Seq[Empty])

  import Extractors._

  object MyExtractors extends Extractors {

    implicit val extractEmpty: Extractor[Empty] = extractor0[Empty](_ => Empty())

    implicit val extractSimple: Extractor[Simple] = extractor1(Simple)

    implicit val extractEmpties: Extractor[Seq[Empty]] = extractorSeq[Empty]("empty")

    implicit val extractDocument: Extractor[Document] = extractor1(Document)
  }


  behavior of "ExtractorsTest"

  it should "extractorSeq" in {
    implicit val ee: Extractor[Empty] = MyExtractors.extractor0[Empty](_ => Empty())
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    val extractedSeq: Try[Seq[Empty]] = MyExtractors.extractorSeq[Empty]("empty").extract(xml)
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

  it should "extractor0" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Empty](_ => Empty()).extract(xml)
    extracted shouldBe Success(Empty())
  }

  it should "extractor1A" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = MyExtractors.extractSimple.extract((xml \ "@id").head)
    extracted shouldBe Success(Simple(1))
  }

  it should "extractor1B" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument.extract(xml)
    extracted shouldBe Success(Document(List(Empty(), Empty())))
  }

}
