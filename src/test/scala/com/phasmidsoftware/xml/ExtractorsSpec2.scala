package com.phasmidsoftware.xml

import com.phasmidsoftware.kmldoc.KmlRenderers
import com.phasmidsoftware.render.{Format, FormatXML, Renderable, Renderers, StateR}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.reflect.ClassTag
import scala.util.{Success, Try, Using}
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

        val names: List[String] = List("_id")
    }

    case class Simple($: String)(val base: Base) extends Base(base._id)

    import Extractors._

    object MyExtractors extends Extractors {

        /**
         * Method to yield an Extractor[B => T] which will be used to extract the properties
         * of T which do NOT belong to B.
         *
         * @param extractorBT an extractor for the type B => T.
         * @tparam B the supertype of T.
         * @tparam T the underlying type of the resulting extractor.
         * @return an Extractor[T].
         */
        def extractorSuper[B >: T : Extractor, T <: Product : ClassTag](extractorBT: Extractor[B => T]): Extractor[T] =
            (node: Node) => {
                val qy: Try[B => T] = extractorBT.extract(node)
                val by: Try[B] = implicitly[Extractor[B]].extract(node)
                for (q <- qy; b <- by) yield q(b)
            }

        implicit val extractorBase: Extractor[Base] = extractor10[Int, Base](Base.apply, Base.names)
        implicit val extractorSimple: Extractor[Simple] = extractorSuper[Base, Simple](extractor10B(Simple.apply, dropLast = true))
    }

//    object MyRenderers extends Renderers {
//
//        def rendererSuper[B >: R : Renderable, R <: Product : ClassTag](rendererBR: Renderable[B => R], lens: R => B): Renderable[R] = new Renderable[R] {
//            def render(r: R, format: Format, stateR: StateR): String = {
//                val w: String = implicitly[Renderable[B]].render(lens(r), format, stateR)
//w
//            }
//        }
//
//        implicit val renderableBase: Renderable[Base] = renderer1[Int, Base](Base.apply)
//        val renderableB: Renderable[Base => Simple] = renderer1B(Simple.apply)
//        implicit val renderableSimple: Renderable[Simple] = rendererSuper[Base, Simple](renderableB, s => s.base)
//    }

    behavior of "Extractors1"

    it should "extract normal attribute" in {
        val xml: Elem = <simple id="2">Robin</simple>
        val extracted: Try[Simple] = MyExtractors.extractorSimple.extract(xml)
        extracted.isSuccess shouldBe true
        extracted.get.$ shouldBe "Robin"
        extracted.get._id shouldBe 2
//        val wy = Using(StateR())(sr => MyRenderers.renderableSimple.render(extracted.get, FormatXML(0), sr))
//        wy shouldBe Success("")
    }
}
