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

  /**
   * A case object for testing using extractor0.
   */
  case object Empty1

  /**
   * A case class with zero members for testing using extractor0.
   */
  case class Empty2()

  /**
   * Case class for testing using extractor1.
   *
   * @param empties a list of Empty1 objects.
   */
  case class Document1(empties: Seq[Empty1.type])

  /**
   * Case class similar to Document1, but has two members.
   *
   * @param _id     the identifier of this Document2.
   *                NOTE that the member name starts with an "_" in order that it be treated as an attribute.
   * @param empties a sequence of Empty1 objects.
   */
  case class Document2(_id: Int, empties: Seq[Empty1.type])

  /**
   * Companion object for Document2.
   * This allows us to have more control over how we construct an instance of Document2.
   */
  object Document2 {
    def apply(id: String, empties: Seq[Empty1.type]): Document2 = Document2(id.toInt, empties)
  }

  /**
   * Case class similar to Document1, but has two members.
   *
   * @param _id     the identifier of this Document2.
   *                NOTE that the member name starts with an "_" in order that it be treated as an attribute.
   * @param empties a sequence of Empty1 objects.
   */
  case class Document3(_id: Int, empties: Seq[Empty1.type])

  import Extractors._

  object MyExtractors extends Extractors {

    implicit val extractEmpty1: Extractor[Empty1.type] = extractor0[Empty1.type](_ => Empty1)

    implicit val extractEmpty2: Extractor[Empty2] = extractor0[Empty2](_ => Empty2())

    implicit val extractEmpties: Extractor[Seq[Empty1.type]] = extractorSequence[Empty1.type]("empty")

    implicit val extractDocument1: Extractor[Document1] = extractor1(Document1)

    val makeDocument2: (String, Seq[Empty1.type]) => Document2 = Document2.apply _
    implicit val extractDocument2: Extractor[Document2] = extractor2(makeDocument2)

    implicit val extractDocument3: Extractor[Document3] = extractor2(Document3)
  }

  behavior of "Extractors$"

  it should "extractSequence" in {
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    import MyExtractors._
    val extractedSeq: Try[Seq[Empty1.type]] = extractSequence[Empty1.type](xml \ "empty")
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

  it should "extractSingleton" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = extractSingleton[Int](xml \ "@id")
    extracted shouldBe Success(1)
  }

  it should "extractor0A" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Empty1.type](_ => Empty1).extract(xml)
    extracted shouldBe Success(Empty1)
  }

  it should "extractor0B" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Empty2](_ => Empty2()).extract(xml)
    extracted shouldBe Success(Empty2())
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
    val extracted = MyExtractors.extractDocument1.extract(xml)
    extracted shouldBe Success(Document1(List(Empty1, Empty1)))
  }

  it should "extractor2A" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument2.extract(xml)
    extracted shouldBe Success(Document2(1, List(Empty1, Empty1)))
  }

  it should "extractor2B" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument3.extract(xml)
    extracted shouldBe Success(Document3(1, List(Empty1, Empty1)))
  }

  it should "match attribute" in {
    Extractors.attribute.matches("_id") shouldBe true
    val z: Matcher = Extractors.attribute.pattern.matcher("_id")
    z.matches() shouldBe true
    z.groupCount() shouldBe 1
    z.group(0) shouldBe "_id"
    z.group(1) shouldBe "id"
  }

  it should "match plural" in {
    Extractors.plural.matches("xs") shouldBe true
    val z: Matcher = Extractors.plural.pattern.matcher("xs")
    z.matches() shouldBe true
    z.groupCount() shouldBe 1
    z.group(0) shouldBe "xs"
    z.group(1) shouldBe "x"
  }

  it should "extractField" in {
    // TODO make extractField private.
    //    val extractors: Extractors.type = Extractors
    //    val extractField = PrivateMethod[Node => Try[Int]](Symbol("extractField"))
    //    val z: Node => Try[Int] = extractors invokePrivate extractField("_id")
    val z: Node => Try[Int] = Extractors.extractField[Int]("_id")
    z(<xml id="1"></xml>) shouldBe Success(1)
  }

  behavior of "Extractors"

  it should "extractorSequence" in {
    implicit val ee: Extractor[Empty1.type] = MyExtractors.extractor0[Empty1.type](_ => Empty1)
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    val extractedSeq: Try[Seq[Empty1.type]] = MyExtractors.extractorSequence[Empty1.type]("empty").extract(xml)
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

}
