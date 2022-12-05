package com.phasmidsoftware.xml

import com.phasmidsoftware.core.WithSuper
import com.phasmidsoftware.render._
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.reflect.ClassTag
import scala.util.{Success, Try, Using}
import scala.xml.{Elem, Node}

class ExtractorsSpec2 extends AnyFlatSpec with should.Matchers with PrivateMethodTester {
    case class Base( _id: Int)

    case class Simple($: String)(val superObject: Base) extends WithSuper[Simple, Base]

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
        def extractorSuper[B <: Product: Extractor, T <: Product with WithSuper[T, B] : ClassTag](extractorBT: Extractor[B => T]): Extractor[T] =
            (node: Node) => {
                val qy: Try[B => T] = extractorBT.extract(node)
                val by: Try[B] = implicitly[Extractor[B]].extract(node)
                for (q <- qy; b <- by) yield q(b)
            }

        implicit val extractorBase: Extractor[Base] = extractor10[Int, Base](Base)
        implicit val extractorSimple: Extractor[Simple] = extractorSuper[Base, Simple](extractor10B(Simple.apply, dropLast = true))
    }

    import Renderers._

    object MyRenderers extends Renderers {
        implicit val renderableBase: Renderable[Base] = renderer1[Int, Base](Base.apply)
        implicit val renderableSimple: Renderable[Simple] = renderer1Super(Simple.apply)
    }

    behavior of "Extractors"

    it should "extract normal attribute" in {
        val xml: Elem = <simple id="2">Robin</simple>
        val extracted: Try[Simple] = MyExtractors.extractorSimple.extract(xml)
        extracted.isSuccess shouldBe true
        extracted.get.$ shouldBe "Robin"
        extracted.get.superObject._id shouldBe 2
    }

    behavior of "Renderers"

    it should "render Base" in {
        import MyRenderers._
        val base = Base(2)
        val wy = Using(StateR())(sr => implicitly[Renderable[Base]].render(base, FormatXML(0), sr))
        wy shouldBe Success("""<Base id="2" ></Base>""")
    }

    it should "render" in {
        import MyRenderers._
        val simple = Simple("Robin")(Base(2))
        println(s"element 0: ${simple.productElement(0)}")
        val renderer: Renderable[Simple] = implicitly[Renderable[Simple]]
        val wy = Using(StateR())(sr => renderer.render(simple, FormatXML(0), sr))
        wy shouldBe Success("""<Simple id="2">Robin</Simple>""")
    }
}
