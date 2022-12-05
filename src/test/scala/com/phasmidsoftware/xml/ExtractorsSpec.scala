package com.phasmidsoftware.xml

import java.util.regex.Matcher
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, Node}

class ExtractorsSpec extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

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
   * @param empties a sequence of Empty objects.
   */
  case class Document2(_id: Int, empties: Seq[Empty.type])

  /**
   * Case class similar to Document1, but has two members.
   *
   * @param _id     the identifier of this Document2A.
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
   * @param emptys    a sequence of Empty objects.
   * @param maybejunk an optional Junk object.
   */
  case class Document3(_id: Int, maybejunk: Option[Junk], emptys: Seq[Empty.type])

  import Extractors._

  object MyExtractors extends Extractors {

    // XXX this is to demonstrate the usage of the translations feature.
    Extractors.translations += "empties" -> "empty"

    implicit val extractEmpty: Extractor[Empty.type] = extractor0[Empty.type](_ => Empty)
    implicit val extractMultiEmpty: MultiExtractor[Seq[Empty.type]] = multiExtractor[Empty.type]
    implicit val extractJunk: Extractor[Junk] = extractor0[Junk](_ => Junk())
    implicit val extractMaybeJunk: Extractor[Option[Junk]] = extractorOption
    implicit val extractDocument1: Extractor[Document1] = extractor01(Document1)
    implicit val extractMultiDocument1: MultiExtractor[Seq[Document1]] = multiExtractor[Document1]
    val makeDocument2A: (String, Seq[Empty.type]) => Document2A = Document2A.apply _
    implicit val extractDocument2A: Extractor[Document2A] = extractor11(makeDocument2A)
    implicit val extractDocument2: Extractor[Document2] = extractor11(Document2)
    implicit val extractDocument3: Extractor[Document3] = extractor21(Document3)
  }

  behavior of "Extractors$"

  it should "extract normal attribute" in {
    val xml: Elem = <kml id="2.2"></kml>
    import Extractors.StringExtractor
    extractField[String]("_id")(xml) should matchPattern { case Success("2.2") => }
  }

  it should "not extract reserved attribute" in {
    val xml: Elem = <kml xmlns="http://www.opengis.net/kml/2.2"></kml>
    import Extractors.StringExtractor
    extractField[String]("_xmlns")(xml) should matchPattern { case Failure(_) => }
  }

  it should "not extract plural attribute" in {
    val xml: Elem = <kml xmlns="http://www.opengis.net/kml/2.2"></kml>
    import MyExtractors.extractEmpty
    extractField[Empty.type]("documents")(xml) should matchPattern { case Failure(_) => }
  }

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
  it should "extractSingleton2" in {
    val xml: Elem = <xml id="A"></xml>
    val extracted = extractSingleton[String](xml \ "@id")
    extracted shouldBe Success("A")
  }

  it should "extractOptional1" in {
    val xml: Elem = <xml>
      <empty></empty>
    </xml>
    import MyExtractors.extractEmpty
    val extracted = extractOptional[Empty.type](xml \ "empty")
    extracted shouldBe Success(Empty)
  }

  it should "extractOptional2" in {
    val xml: Elem = <xml>
    </xml>
    import MyExtractors.extractEmpty
    val extracted = extractOptional[Empty.type](xml \ "empty")
    extracted shouldBe Success(None)
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

  it should "extractor10A" in {
    val xml: Elem = <xml>
      <id>1</id>
    </xml>
    val extracted = MyExtractors.extractor10(Simple1).extract(xml)
    extracted shouldBe Success(Simple1(1))
  }

  it should "extractor10Super" in {
    val xml: Elem = <xml id="1"></xml>
    val extracted = MyExtractors.extractor10(Simple2).extract(xml)
    extracted shouldBe Success(Simple2("1"))
  }

  it should "multiExtractor[Document1]" in {
    val xml: Elem = <xml>
      <document1>
        <empty></empty> <empty></empty>
      </document1>
    </xml>
    import MyExtractors._
    val extracted: Try[Seq[Document1]] = extractMultiDocument1.extract(xml \ "document1")
    extracted shouldBe Success(List(Document1(List(Empty, Empty))))
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
    extracted shouldBe Success(Document3(1, None, List(Empty, Empty)))
  }

  it should "extractor3B" in {
    val xml: Elem = <xml id="1">
      <empty></empty> <empty></empty>
      <junk></junk>
    </xml>
    val extracted = MyExtractors.extractDocument3.extract(xml)
    extracted shouldBe Success(Document3(1, Some(Junk()), List(Empty, Empty)))
  }

  it should "match attribute" in {
    Extractors.attribute.matches("_id") shouldBe true
    val matcher: Matcher = Extractors.attribute.pattern.matcher("_id")
    matcher.matches() shouldBe true
    matcher.groupCount() shouldBe 1
    matcher.group(0) shouldBe "_id"
    matcher.group(1) shouldBe "id"
  }

  it should "match plural" in {
    Extractors.plural.matches("xs") shouldBe true
    val matcher: Matcher = Extractors.plural.pattern.matcher("xs")
    matcher.matches() shouldBe true
    matcher.groupCount() shouldBe 1
    matcher.group(0) shouldBe "xs"
    matcher.group(1) shouldBe "x"
  }

  it should "match optional" in {
    Extractors.optional.matches("xs") shouldBe false
    val matcher: Matcher = Extractors.optional.pattern.matcher("maybexs")
    matcher.matches() shouldBe true
    matcher.groupCount() shouldBe 1
    matcher.group(0) shouldBe "maybexs"
    matcher.group(1) shouldBe "xs"
  }

  it should "extractField String" in {
    val we: Node => Try[String] = Extractors.extractField[String]("_id")
    we(<xml id="xyz"></xml>) shouldBe Success("xyz")
  }

  it should "extractField Int" in {
    val ie: Node => Try[Int] = Extractors.extractField[Int]("_id")
    ie(<xml id="1"></xml>) shouldBe Success(1)
  }

  it should "extractField Boolean" in {
    val be: Node => Try[Boolean] = Extractors.extractField[Boolean]("_ok")
    be(<xml ok="true"></xml>) shouldBe Success(true)
  }

  it should "extractField Double" in {
    val de: Node => Try[Double] = Extractors.extractField[Double]("_weight")
    de(<xml weight="42.0"></xml>) shouldBe Success(42)
  }

  it should "extractField Long" in {
    val le: Node => Try[Long] = Extractors.extractField[Long]("_id")
    le(<xml id="42"></xml>) shouldBe Success(42)
  }

  behavior of "Extractors"

  // TODO add tests for extractor1, etc.

  it should "extractorSequence" in {
    implicit val ee: Extractor[Empty.type] = MyExtractors.extractor0[Empty.type](_ => Empty)
    val xml: Elem = <xml>
      <empty></empty> <empty></empty>
    </xml>
    val extractedSeq: Try[Seq[Empty.type]] = MyExtractors.extractorSequence[Empty.type]("empty").extract(xml)
    extractedSeq should matchPattern { case Success(_ :: _ :: Nil) => }
  }

}
