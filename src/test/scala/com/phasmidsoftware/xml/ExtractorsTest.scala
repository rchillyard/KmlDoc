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
  case object Empty

  /**
   * A case class with zero members for testing using extractor0.
   */
  case class Junk()

  /**
   * Case class for testing using extractor1.
   *
   * @param empties a list of Empty objects.
   */
  case class Document1(empties: Seq[Empty.type])

  /**
   * Case class similar to Document1, but has two members.
   *
   * @param _id     the identifier of this Document2A.
   *                NOTE that the member name starts with an "_" in order that it be treated as an attribute.
   * @param empties a sequence of Empty objects.
   */
  case class Document2(_id: Int, empties: Seq[Empty.type])

  /**
   * Case class similar to Document1, but has two members.
   *
   * @param _id     the identifier of this Document2A.
   *                NOTE that the member name starts with an "_" in order that it be treated as an attribute.
   * @param empties a sequence of Empty objects.
   */
  case class Document2A(_id: Int, empties: Seq[Empty.type])

  /**
   * Companion object for Document2A.
   * This allows us to have more control over how we construct an instance of Document2A.
   */
  object Document2A {
    def apply(id: String, empties: Seq[Empty.type]): Document2A = Document2A(id.toInt, empties)
  }

  /**
   * Case class similar to Document1, but has two members.
   *
   * @param _id       the identifier of this Document2A.
   *                  NOTE that the member name starts with an "_" in order that it be treated as an attribute.
   * @param emptys    a sequence of Empty objects.
   * @param maybejunk an optional Junk object.
   */
  case class Document3(_id: Int, emptys: Seq[Empty.type], maybejunk: Option[Junk])

  import Extractors._

  object MyExtractors extends Extractors {

    implicit val extractEmpty1: Extractor[Empty.type] = extractor0[Empty.type](_ => Empty)

    implicit val extractEmpty2: Extractor[Junk] = extractor0[Junk](_ => Junk())

    implicit val extractEmpties: Extractor[Seq[Empty.type]] = extractorSequence[Empty.type]("empty")

    //    implicit val extractMaybeJunk: Extractor[Option[Junk]] = extractorOptional[Junk]("junk")
    implicit val extractMaybeJunk: Extractor[Option[Junk]] = extractorOption[Junk]("junk")

    implicit val extractDocument1: Extractor[Document1] = extractor1(Document1)

    val makeDocument2A: (String, Seq[Empty.type]) => Document2A = Document2A.apply _
    implicit val extractDocument2A: Extractor[Document2A] = extractor2(makeDocument2A)

    implicit val extractDocument2: Extractor[Document2] = extractor2(Document2)

    implicit val extractDocument3: Extractor[Document3] = extractor3(Document3)
  }

  behavior of "Extractors$"

  it should "extractSequence" in {
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    import MyExtractors._
    val extractedSeq: Try[Seq[Empty.type]] = extractSequence[Empty.type](xml \ "empty")
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

  it should "extractSingleton" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = extractSingleton[Int](xml \ "@id")
    extracted shouldBe Success(1)
  }

  it should "extractor0A" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Empty.type](_ => Empty).extract(xml)
    extracted shouldBe Success(Empty)
  }

  it should "extractor0B" in {
    val xml: Elem = <xml></xml>
    val extracted = MyExtractors.extractor0[Junk](_ => Junk()).extract(xml)
    extracted shouldBe Success(Junk())
  }

  it should "extractor1A" in {
    val xml: Elem = <xml>
      <id>1</id>
    </xml>
    val extracted = MyExtractors.extractor1(Simple1).extract(xml)
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
    extracted shouldBe Success(Document1(List(Empty, Empty)))
  }

  it should "extractor2A" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument2A.extract(xml)
    extracted shouldBe Success(Document2A(1, List(Empty, Empty)))
  }

  it should "extractor2B" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument2.extract(xml)
    extracted shouldBe Success(Document2(1, List(Empty, Empty)))
  }

  it should "extractor3A" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
    </xml>
    val extracted = MyExtractors.extractDocument3.extract(xml)
    extracted shouldBe Success(Document3(1, List(Empty, Empty), None))
  }

  it should "extractor3B" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
      <junk></junk>
    </xml>
    val extracted = MyExtractors.extractDocument3.extract(xml)
    extracted shouldBe Success(Document3(1, List(Empty, Empty), Some(Junk())))
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
    val z: Node => Try[Int] = Extractors.extractField[Int]("_id")
    z(<xml id="1"></xml>) shouldBe Success(1)
  }

  //  it should "extractFieldSequence" in {
  //    implicit val ee: Extractor[Empty.type] = MyExtractors.extractor0[Empty.type](_ => Empty)
  //    val z: Node => Try[Seq[Empty.type]] = Extractors.extractFieldSequence[Empty.type]("emptys")
  //    z(<xml><empty></empty></xml>) shouldBe Success(Seq(Empty))
  //  }

  behavior of "Extractors"

  it should "extractorSequence" in {
    implicit val ee: Extractor[Empty.type] = MyExtractors.extractor0[Empty.type](_ => Empty)
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    val extractedSeq: Try[Seq[Empty.type]] = MyExtractors.extractorSequence[Empty.type]("empty").extract(xml)
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

}
