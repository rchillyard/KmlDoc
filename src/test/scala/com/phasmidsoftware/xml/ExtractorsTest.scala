package com.phasmidsoftware.xml

import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.regex.Matcher
import scala.util.{Success, Try}
import scala.xml.{Elem, Node}

class ExtractorsTest extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

  case class Simple1(id: Int)

  case class Simple2(_id: String)

  case class Empty()

  case class Document(empties: Seq[Empty])
  //  case class Document(id: Int, empties: Seq[Empty])

  import Extractors._

  object MyExtractors extends Extractors {

    implicit val extractEmpty: Extractor[Empty] = extractor0[Empty](_ => Empty())

    implicit val extractEmpties: Extractor[Seq[Empty]] = extractorSequence[Empty]("empty")

    implicit val extractDocument: Extractor[Document] = extractor1(Document)
  }

  behavior of "Extractors$"

  it should "extractSequence" in {
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    import MyExtractors._
    val extractedSeq: Try[Seq[Empty]] = extractSequence[Empty](xml \ "empty")
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

  it should "extractSingleton" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = extractSingleton[Int](xml \ "@id")
    extracted shouldBe Success(1)
  }

  it should "extractor0" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Empty](_ => Empty()).extract(xml)
    extracted shouldBe Success(Empty())
  }

  it should "extractor1A" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = MyExtractors.extractor1(Simple1).extract((xml \ "@id").head)
    extracted shouldBe Success(Simple1(1))
  }

  it should "extractor1B" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = MyExtractors.extractor1(Simple2).extract(xml)
    extracted shouldBe Success(Simple2("1"))
  }

  it should "extractor1C" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument.extract(xml)
    extracted shouldBe Success(Document(List(Empty(), Empty())))
  }

  it should "match " in {
    Extractors.attribute.matches("_id") shouldBe true
    val z: Matcher = Extractors.attribute.pattern.matcher("_id")
    z.matches() shouldBe true
    z.groupCount() shouldBe 2
    z.group(0) shouldBe "_id"
    z.group(1) shouldBe "_id"
    z.group(2) shouldBe "id"
  }

  it should "extractField" in {
    //    val extractors: Extractors.type = Extractors
    //    val extractField = PrivateMethod[Node => Try[Int]](Symbol("extractField"))
    //    val z: Node => Try[Int] = extractors invokePrivate extractField("_id")
    val z: Node => Try[Int] = Extractors.extractField[Int]("_id")
    z(<xml id="1"></xml>) shouldBe Success(1)
  }

  behavior of "Extractors"

  it should "extractorSequence" in {
    implicit val ee: Extractor[Empty] = MyExtractors.extractor0[Empty](_ => Empty())
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    val extractedSeq: Try[Seq[Empty]] = MyExtractors.extractorSequence[Empty]("empty").extract(xml)
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

}
