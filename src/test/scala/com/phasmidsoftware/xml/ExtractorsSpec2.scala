package com.phasmidsoftware.xml

import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.reflect.ClassTag
import scala.util.Try
import scala.xml.{Elem, Node}

class ExtractorsSpec2 extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

    class Base(val _id: Int) extends Product {
        def productArity: Int = 1

        def productElement(n: Int): Any = Base.names(n)

        override def productElementNames: Iterator[String] = Base.names.iterator

        def canEqual(that: Any): Boolean = that.isInstanceOf[Base]

        override def equals(obj: Any): Boolean = obj match {
            case Base(x) => x == _id
            case _ => false
        }
    }

    object Base {
        def apply(x: Int): Base = new Base(x)

        def unapply(base: Base): Option[Int] =
            if (base eq null) None
            else Some(base._id)

        val names = List("_id")
    }

    case class Simple($: String)(base: Base) extends Base(base._id)

    import Extractors._

    object MyExtractors extends Extractors {

        def extractorSuper[B >: T : Extractor, T <: Product : ClassTag](extractorBT: Extractor[B => T]): Extractor[T] = new Extractor[T] {
            def extract(node: Node): Try[T] = {
                val qy: Try[B => T] = extractorBT.extract(node)
                val by: Try[B] = implicitly[Extractor[B]].extract(node)
                for (q <- qy; b <- by) yield q(b)
            }
        }

        implicit val extractorBase: Extractor[Base] = extractor10[Int, Base](Base.apply, Base.names)
        implicit val extractorSimple: Extractor[Simple] = extractorSuper[Base, Simple](extractor10B(Simple.apply))
    }

    behavior of "Extractors1"

    it should "extract normal attribute" in {
        val xml: Elem = <simple id="2">Robin</simple>
        val extracted: Try[Simple] = MyExtractors.extractorSimple.extract(xml)
        extracted.isSuccess shouldBe true
        extracted.get.$ shouldBe "Robin"
        extracted.get._id shouldBe 2

    }
}
