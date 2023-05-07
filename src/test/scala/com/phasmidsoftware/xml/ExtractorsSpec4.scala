package com.phasmidsoftware.xml

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Try


trait Animal

object Animal extends Extractors {
    implicit val multiExtractor: MultiExtractor[Seq[Animal]] = multiExtractor2[Animal, (Dog, Cat), Dog, Cat]((d, c) => (d, c), Seq("dog", "cat"))
}

case class Dog(_name: String) extends Animal

object Dog extends Extractors {
    implicit val extractor: Extractor[Dog] = extractor10(Dog.apply)
}

case class Cat(_name: String) extends Animal

object Cat extends Extractors {
    implicit val extractor: Extractor[Cat] = extractor10(Cat.apply)
}


class ExtractorsSpec4 extends AnyFlatSpec with Extractors with should.Matchers {
    behavior of "Extractors"

    it should "extractor01" in {

    }
    it should "extractor02" in {

    }

    it should "extractor03" in {

    }

    it should "extractor04" in {

    }

    it should "extractor05" in {

    }

    it should "extractor06" in {

    }

    it should "extractor40" in {

    }

    it should "extractor30" in {

    }

    it should "extractor31" in {

    }

    it should "extractor41" in {

    }

    it should "extractor42" in {

    }

    it should "extractor50" in {

    }

    it should "extractorIterable" in {

    }

    it should "extractorPartial0" in {

    }

    it should "extractorPartial14" in {

    }

    it should "extractorPartial20" in {

    }

    it should "extractorPartial21" in {

    }

    it should "extractorPartial22" in {

    }

    it should "extractorPartial23" in {

    }

    it should "extractorPartial60" in {

    }

    it should "multiExtractor" in {

    }

    it should "deprecatedChildrenExtractor" in {

    }

    it should "extractorPartial" in {

    }

    it should "extractorSubtype" in {

    }

    it should "extractorPartial50" in {

    }

    it should "extractorPartial51" in {

    }

    it should "extractorPartial10" in {

    }

    it should "extractorPartial11" in {

    }

    it should "extractorPartial12" in {

    }

    it should "extractorPartial13" in {

    }

    it should "extractorPartial03" in {

    }

    it should "extractorPartial04" in {

    }

    it should "extractor32" in {

    }

    it should "extractorPartial05" in {

    }

    it should "lazyMultiExtractor" in {

    }

    it should "extractor33" in {

    }

    it should "lazyExtractor" in {

    }

    it should "extractor0" in {

    }

    it should "extractorPartial40" in {

    }

    it should "extractorPartial41" in {

    }

    it should "extractorAlt" in {

    }

    it should "extractorPartial01" in {

    }

    it should "extractorPartial02" in {

    }

    it should "multiExtractor5" in {

    }

    it should "extractor20" in {

    }

    it should "multiExtractor6" in {

    }

    it should "extractor21" in {

    }

    it should "extractorOption" in {

    }

    it should "multiExtractor3" in {

    }

    it should "multiExtractor4" in {

    }

    it should "extractor24" in {

    }

    it should "extractor22" in {

    }

    it should "extractor23" in {

    }

    it should "multiExtractor1" in {

    }

    it should "subclassExtractor1" in {
    }

    it should "multiExtractor2" in {

    }

    it should "subclassExtractor2" in {
        val subclassExtractor = subclassExtractor2[Animal, (Dog, Cat), Dog, Cat]((d, c) => (d, c), Seq("dog", "cat"))
        val animals = <animals>
            <dog name="Xena"></dog> <cat name="Puss"></cat>
        </animals>
        val asy: Try[Seq[Animal]] = subclassExtractor.extract(animals)
        asy.isSuccess shouldBe true
        asy.get shouldBe Seq(Dog("Xena"), Cat("Puss"))
    }

    it should "multiExtractorBase" in {

    }

    it should "extractorPartial30" in {

    }

    it should "extractorPartial31" in {

    }

    it should "extractorPartial32" in {

    }

    it should "extractor60" in {

    }

    it should "extractor10" in {

    }

    it should "extractor51" in {

    }

    it should "extractorAlia3" in {

    }

    it should "extractor13" in {

    }

    it should "extractorAlia4" in {

    }

    it should "extractor14" in {

    }

    it should "extractorAlia5" in {

    }

    it should "extractor11" in {

    }

    it should "extractorAlia6" in {

    }

    it should "fieldExtractor" in {

    }

    it should "extractor12" in {

    }

    it should "extractor15" in {

    }

    it should "flog" in {

    }

    it should "extractSingleton" in {

    }

    it should "extractOptional" in {

    }

    it should "extractSequence" in {

    }

    it should "extractorOptionalString" in {

    }

    it should "extractOptionalInt" in {

    }

}
