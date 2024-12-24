package com.phasmidsoftware.xml

import com.phasmidsoftware.core.FP
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Try}
import scala.xml.Node

class ExtractorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Extractor"

  it should "extract" in {
    val target: Extractor[Int] = (node: Node) => FP.optionToTry(node.text.toIntOption, new NoSuchElementException())
    target.extract(<junk>1</junk>) shouldBe Try(1)
    target.extract(<junk>one</junk>) should matchPattern { case Failure(_) => }
  }

  it should "map" in {
    val node = <junk>1</junk>
    val ext: Extractor[Int] = (node: Node) => FP.optionToTry(node.text.toIntOption, new NoSuchElementException())
    val target: Extractor[Int] = ext.map(x => x + 1)
    target.extract(node) shouldBe Try(2)
  }

  it should "flatMap" in {
    val node = <junk>1</junk>
    val ext: Extractor[Int] = (node: Node) => FP.optionToTry(node.text.toIntOption, new NoSuchElementException())
    val target: Extractor[Int] = ext.flatMap(x => Try(x + 1))
    target.extract(node) shouldBe Try(2)
  }

  it should "$bar" in {

  }

  it should "optionalAttribute" in {

  }

  it should "plural" in {

  }

  it should "logger" in {

  }

  it should "createLazy" in {

  }

  it should "booleanExtractor" in {

  }

  it should "doubleExtractor" in {

  }

  it should "extractElementsByLabel" in {

  }

  it should "inferAttributeType" in {

  }

  it should "attribute" in {

  }

  it should "apply1" in {

  }

  it should "apply2" in {

  }

  it should "optional" in {

  }

  it should "extractAll" in {

  }

  it should "unitExtractor" in {

  }

  it should "extractSingleton" in {

  }

  it should "extractMulti" in {

  }

  it should "longExtractor" in {

  }

  it should "fieldExtractor" in {

  }

  it should "extractChildren" in {

  }

  it should "intExtractor" in {

  }

  it should "extractSequence" in {

  }

}
