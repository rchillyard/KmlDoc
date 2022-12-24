package com.phasmidsoftware.render

import com.phasmidsoftware.core.FP.tryNotNull
import com.phasmidsoftware.core.{Text, TryUsing, XmlException}
import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.kmldoc.KmlRenderers.optionRenderer
import com.phasmidsoftware.xml.{Extractor, NamedFunction}
import org.slf4j.{Logger, LoggerFactory}
import scala.annotation.unused
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

/**
 * Trait which defines generic and standard renderers.
 *
 */
trait Renderers {

    val flog: Flog = Flog[Renderers]

    /**
     * Method to create a renderer fpr a case class with no members, or a case object.
     *
     * @tparam R the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer0[R <: Product : ClassTag]: Renderable[R] = Renderable {
        (_: R, format: Format, stateR: StateR) =>
            doNestedRender(format, stateR, "", "", "")
    } ^^ s"renderer0: ${implicitly[ClassTag[R]]}"

    /**
     * Alternative method to create a renderer fpr a Product (e.g., case class) with one member but also an auxiliary object in a second parameter set.
     *
     * CONSIDER rename each of theses renderXSuper methods to renderXAux.
     *
     * @param construct a function which takes a P0 and yields a function of B => R
     *                  (this is usually the apply method of a case class that has a second parameter set with one parameter of type B).
     * @tparam B the (Renderable) type of the auxiliary object of type R.
     * @tparam R the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer0Super[B: Renderable, R <: Product : ClassTag](construct: B => R)(lens: R => B): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) =>
            val b = lens(r)
            for {br <- tryNotNull(implicitly[Renderable[B]])("renderer0Super: Renderable[B]")
                 wInner <- br.render(b, format.indent, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, "", r.productElementName(0))
                 } yield result
    } ^^ s"renderer0Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with one member.
     *
     * @param ignored (unused) a function which takes a P0 and yields an R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderable) type of the (single) member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer1[P0: Renderable, R <: Product : ClassTag](@unused ignored: P0 => R): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) =>
            for {wOuter <- renderOuter(r, r.productElement(0).asInstanceOf[P0], 0, format)
                 result <- doNestedRender(format, stateR, "", wOuter, r.productElementName(0))
                 } yield result
    } ^^ s"renderer1: ${implicitly[ClassTag[R]]}"

    /**
     * Alternative method to create a renderer fpr a Product (e.g., case class) with one member but also an auxiliary object in a second parameter set.
     *
     * @param construct a function which takes a P0 and yields a function of B => R
     *                  (this is usually the apply method of a case class that has a second parameter set with one parameter of type B).
     * @tparam B  the (Renderable) type of the auxiliary object of type R.
     * @tparam P0 the (Renderable) type of the (single) member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer1Super[B: Renderable, P0: Renderable, R <: Product : ClassTag](construct: P0 => B => R)(lens: R => B): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) =>
            val b = lens(r)
            val constructOuter: P0 => R = construct(_)(b)
            for {
                br <- tryNotNull(implicitly[Renderable[B]])("renderer1Super: Renderable[B]")
                wInner <- br.render(b, format.indent, stateR.recurse)
                wOuter <- renderer1(constructOuter).render(r, format.indent, stateR.recurse)
                result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
            } yield result
    } ^^ s"renderer1Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with two members.
     *
     * @param construct a function (P0, P1) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a function which takes an R, a Format, and a StateR as parameters and yields a Renderable[R].
     */
    def renderer2[P0: Renderable, P1: Renderable, R <: Product : ClassTag](construct: (P0, P1) => R): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) =>
            val objectOuter = r.productElement(1).asInstanceOf[P1]
            val constructorInner: P0 => R = construct(_, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0])
            for {wInner <- renderer1(constructorInner).render(objectInner, format.indent, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 1, format)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(1))
                 } yield result
    } ^^ s"renderer2: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with two members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderable) type of the auxiliary object of type R.
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer2Super[B: Renderable, P0: Renderable, P1: Renderable, R <: Product : ClassTag](construct: (P0, P1) => B => R)(lens: R => B): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1) => R = construct(_, _)(b)
            for {br <- tryNotNull(implicitly[Renderable[B]])("renderer2Super: Renderable[B]")
                 wInner <- br.render(b, format.indent, stateR.recurse)
                 wOuter <- renderer2(constructOuter).render(r, format.indent, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
                 } yield result
        }
    } ^^ s"renderer2Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with three members.
     *
     * @param construct a function (P0, P1, P2) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer3[P0: Renderable, P1: Renderable, P2: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2) => R): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(2).asInstanceOf[P2]
            val constructorInner: (P0, P1) => R = construct(_, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1])
            for {wInner <- renderer2(constructorInner).render(objectInner, format.indent, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 2, format)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(2))
                 } yield result
        }
    } ^^ s"renderer3: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with three members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderable) type of the auxiliary object of type R.
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer3Super[B: Renderable, P0: Renderable, P1: Renderable, P2: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2) => B => R)(lens: R => B): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2) => R = construct(_, _, _)(b)
            for {br <- tryNotNull(implicitly[Renderable[B]])("renderer3Super: Renderable[B]")
                 wInner <- br.render(b, format.indent, stateR.recurse)
                 wOuter <- renderer3(constructOuter).render(r, format.indent, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
                 } yield result
        }
    } ^^ s"renderer3Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with four members.
     *
     * @param construct a function (P0, P1, P2, P3) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam P3 the (Renderable) type of the fourth member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return Renderable[R].
     */
    def renderer4[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3) => R): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(3).asInstanceOf[P3]
            val constructorInner: (P0, P1, P2) => R = construct(_, _, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2])
            for {wInner <- renderer3(constructorInner).render(objectInner, format.indent, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 3, format)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(3))
                 } yield result
        }
    } ^^ s"renderer4: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with four members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2, P3) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderable) type of the auxiliary object of type R.
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam P3 the (Renderable) type of the fourth member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer4Super[B: Renderable, P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => R)(lens: R => B): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2, P3) => R = construct(_, _, _, _)(b)
            for {br <- tryNotNull(implicitly[Renderable[B]])("renderer4Super: Renderable[B]")
                 wInner <- br.render(b, format.indent, stateR.recurse)
                 wOuter <- renderer4(constructOuter).render(r, format.indent, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
                 } yield result
        }
    } ^^ s"renderer4Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with five members.
     *
     * @param construct a function (P0, P1, P2, P3, P4) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam P3 the (Renderable) type of the fourth member of Product type R.
     * @tparam P4 the (Renderable) type of the fifth member of Product type R.
     * @tparam R  the (Renderable) type of Renderable to be returned (must be a Product).
     * @return Renderable[R].
     */
    def renderer5[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, P4: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => R): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(4).asInstanceOf[P4]
            val constructorInner: (P0, P1, P2, P3) => R = construct(_, _, _, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2], r.productElement(3).asInstanceOf[P3])
            for {wInner <- renderer4(constructorInner).render(objectInner, format.indent, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 4, format)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(4))
                 } yield result
        }
    } ^^ s"renderer5: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with five members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2, P3, P4) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderable) type of the auxiliary object of type R.
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam P3 the (Renderable) type of the fourth member of Product type R.
     * @tparam P4 the (Renderable) type of the fifth member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
  def renderer5Super[B: Renderable, P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, P4: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => R)(lens: R => B): Renderable[R] = Renderable {
      (r: R, format: Format, stateR: StateR) => {
          val b = lens(r)
          val constructOuter: (P0, P1, P2, P3, P4) => R = construct(_, _, _, _, _)(b)
          for {br <- tryNotNull(implicitly[Renderable[B]])("renderer5Super: Renderable[B]")
               wInner <- br.render(b, format.indent, stateR.recurse)
               wOuter <- renderer5(constructOuter).render(r, format.indent, stateR.recurse)
               result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
               } yield result
      }
  } ^^ s"renderer5Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with five members.
     *
     * @param construct a function which takes a P0, P1, P2, P3, P4 and yields an R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam P3 the (Renderable) type of the fourth member of Product type R.
     * @tparam P4 the (Renderable) type of the fifth member of Product type R.
     * @tparam P5 the (Renderable) type of the sixth member of Product type R.
     * @tparam R  the (Renderable) type of Renderable to be returned (must be a Product).
     * @return Renderable[R].
     */
    def renderer6[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, P4: Renderable, P5: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => R): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(5).asInstanceOf[P5]
            val constructorInner: (P0, P1, P2, P3, P4) => R = construct(_, _, _, _, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2], r.productElement(3).asInstanceOf[P3], r.productElement(4).asInstanceOf[P4])
            for {wInner <- renderer5(constructorInner).render(objectInner, format.indent, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 4, format)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(5))
                 } yield result
        }
    } ^^ s"renderer6: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with six members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2, P3, P4, P5) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderable) type of the auxiliary object of type R.
     * @tparam P0 the (Renderable) type of the first member of Product type R.
     * @tparam P1 the (Renderable) type of the second member of Product type R.
     * @tparam P2 the (Renderable) type of the third member of Product type R.
     * @tparam P3 the (Renderable) type of the fourth member of Product type R.
     * @tparam P4 the (Renderable) type of the fifth member of Product type R.
     * @tparam P5 the (Renderable) type of the sixth member of Product type R.
     * @tparam R  the type of Renderable to be returned (must be a Product).
     * @return a Renderable[R].
     */
    def renderer6Super[B: Renderable, P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, P4: Renderable, P5: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => B => R)(lens: R => B): Renderable[R] = Renderable {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2, P3, P4, P5) => R = construct(_, _, _, _, _, _)(b)
            for {
                br <- tryNotNull(implicitly[Renderable[B]])("renderer5Super: Renderable[B]")
                wInner <- br.render(b, format.indent, stateR.recurse)
                wOuter <- renderer6(constructOuter).render(r, format.indent, stateR.recurse)
                result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
            } yield result
        }
    } ^^ s"renderer6Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to yield a renderer of Option[R].
     *
     * @tparam R the (Renderable) underlying type to be rendered.
     * @return a Renderable of Option[R].
     */
    def optionRenderer[R: Renderable : ClassTag]: Renderable[Option[R]] = Renderable {
        (ro: Option[R], format: Format, stateR: StateR) =>
            ro match {
                case Some(r) =>
                    val wo = stateR.maybeName match {
                        case Some(Extractor.optional(x)) => Some(x.head.toLower + x.tail)
                        case Some(x) => Some(x)
                        case None => None
                    }
                    implicitly[Renderable[R]].render(r, format, StateR(wo))
                case None => Success("")
            }
    } ^^ s"optionRenderer: ${implicitly[ClassTag[R]]}"

    /**
     * Method to yield a Renderable[T] such that the rendering can be performed according to the renderables for one sub-type of T (R0).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @return a Renderable[T].
     */
    def rendererSuper1[T: ClassTag, R0 <: T : Renderable : ClassTag]: Renderable[T] = Renderable {

        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        renderable <- tryNotNull(implicitly[Renderable[R0]])(s"rendererSuper1: ${implicitly[ClassTag[T]]}")
                        result <- renderable.render(r, format, stateR)
                    } yield result
                case _ =>
                    Failure(XmlException(s"rendererSuper1: object of type ${t.getClass} is not a sub-type for ${implicitly[ClassTag[T]]}\n" +
                            s"Are you sure that, in the appropriate rendererSuperN definition, you've included all possible sub-types?"))
            }
    } ^^ s"rendererSuper1: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderable[T] such that the rendering can be performed according to the renderables for two sub-types of T (R0 or R1).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @return a Renderable[T].
     */
    def rendererSuper2[T: ClassTag, R0 <: T : Renderable : ClassTag, R1 <: T : Renderable : ClassTag]: Renderable[T] = Renderable {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        r0r <- tryNotNull(implicitly[Renderable[R0]])(s"rendererSuper2: ${implicitly[ClassTag[T]]}")
                        result <- r0r.render(r, format, stateR)
                    } yield result
                case _ => rendererSuper1[T, R1].render(t, format, stateR)
            }
    } ^^ s"rendererSuper2: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderable[T] such that the rendering can be performed according to the renderables for three sub-types of T (R0, R1, or R2).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @return a Renderable[T].
     */
    def rendererSuper3[T: ClassTag, R0 <: T : Renderable : ClassTag, R1 <: T : Renderable : ClassTag, R2 <: T : Renderable : ClassTag]: Renderable[T] = Renderable {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    val r0ry = tryNotNull(implicitly[Renderable[R0]])(s"rendererSuper3: ${implicitly[ClassTag[T]]}")
                    for (r0r <- r0ry; result <- r0r.render(r, format, stateR)) yield result
                case _ => rendererSuper2[T, R1, R2].render(t, format, stateR)
            }
    } ^^ s"rendererSuper3: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderable[T] such that the rendering can be performed according to the renderables for four sub-types of T (R0, R1, R2, or R3).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @tparam R3 another sub-type of T.
     * @return a Renderable[T].
     */
    def rendererSuper4[T: ClassTag, R0 <: T : Renderable : ClassTag, R1 <: T : Renderable : ClassTag, R2 <: T : Renderable : ClassTag, R3 <: T : Renderable : ClassTag]: Renderable[T] = Renderable {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        r0r <- tryNotNull(implicitly[Renderable[R0]])(s"rendererSuper4: ${implicitly[ClassTag[T]]}")
                        result <- r0r.render(r, format, stateR)
                    } yield result
                case _ => rendererSuper3[T, R1, R2, R3].render(t, format, stateR)
            }
    } ^^ s"rendererSuper4: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderable[T] such that the rendering can be performed according to the renderables for five sub-types of T (R0, R1, R2, R3, or R4).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @tparam R3 another sub-type of T.
     * @tparam R4 another sub-type of T.
     * @return a Renderable[T].
     */
    def rendererSuper5[T: ClassTag, R0 <: T : Renderable : ClassTag, R1 <: T : Renderable : ClassTag, R2 <: T : Renderable : ClassTag, R3 <: T : Renderable : ClassTag, R4 <: T : Renderable : ClassTag]: Renderable[T] = Renderable {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        r0r <- tryNotNull(implicitly[Renderable[R0]])(s"rendererSuper5: ${implicitly[ClassTag[T]]}")
                        result <- r0r.render(r, format, stateR)
                    } yield result
                case _ => rendererSuper4[T, R1, R2, R3, R4].render(t, format, stateR)
            }
    } ^^ s"rendererSuper5: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderable[T] such that the rendering can be performed according to the renderables for six sub-types of T (R0, R1, R2, R3, R4, or R5).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @tparam R3 another sub-type of T.
     * @tparam R4 another sub-type of T.
     * @tparam R5 another sub-type of T.
     * @return a Renderable[T].
     */
    def rendererSuper6[T: ClassTag, R0 <: T : Renderable : ClassTag, R1 <: T : Renderable : ClassTag, R2 <: T : Renderable : ClassTag, R3 <: T : Renderable : ClassTag, R4 <: T : Renderable : ClassTag, R5 <: T : Renderable : ClassTag]: Renderable[T] = Renderable { (t: T, format: Format, stateR: StateR) =>
        t match {
            case r: R0 =>
                for {
                    r0r <- tryNotNull(implicitly[Renderable[R0]])(s"rendererSuper6: ${implicitly[ClassTag[T]]}")
                    result <- r0r.render(r, format, stateR)
                } yield result
            case _ => rendererSuper5[T, R1, R2, R3, R4, R5].render(t, format, stateR)
        }
    } ^^ s"rendererSuper6: ${implicitly[ClassTag[T]]}"

    /**
     * Method to return a Renderable of Seq[R].
     *
     * @tparam R the underlying element type.
     * @return a Renderable of Seq[R].
     */
    def sequenceRenderer[R: Renderable : ClassTag]: Renderable[Seq[R]] = Renderable {
        (rs: Seq[R], format: Format, _: StateR) => doRenderSequence(rs, format, None)
    } ^^ s"sequenceRenderer: ${implicitly[ClassTag[R]]}"

    /**
     * Method to return a Renderable of Seq[R] with a pre-defined format.
     *
     * NOTE This is required for allowing a format to take precedence over the format parameter passed into the render method.
     *
     * @param formatFunc a function which, given the current state of indents, yields the required format to pass into doRenderSequence.
     * @tparam R the underlying type to be rendered.
     * @return a Renderable of Seq[R].
     */
    def sequenceRendererFormatted[R: Renderable : ClassTag](formatFunc: Int => Format): Renderable[Seq[R]] = Renderable {
        (rs: Seq[R], format: Format, stateR: StateR) =>
            doRenderSequence(rs, formatFunc(format.indents), stateR.maybeName)
    } ^^ s"sequenceRendererFormatted: ${implicitly[ClassTag[R]]}"

    private def renderOuter[R <: Product : ClassTag, P: Renderable](r: R, objectOuter: P, indexOuter: Int, format: Format): Try[String] =
        TryUsing(StateR().setName(r, indexOuter)) {
            sr =>
                for {p <- tryNotNull(implicitly[Renderable[P]])(s"renderOuter: ${implicitly[ClassTag[R]]}")
                     x <- p.render(objectOuter, format.indent, sr)
                     } yield sr.getAttributes + x
        }

    /**
     * Method which is called at the end of each renderN method (above).
     * Its purpose is to combine the textual information held in the following three parameters:
     * <ol>
     * <li>stateR: attributes are placed here temporarily;</li>
     * <li>wInner: a String generated by a call to a renderN method with the next lower number;</li>
     * <li>wOuter: a String based on the last member of this Product being rendered.</li>
     *
     * @param format        the format in which the Product should be rendered.
     * @param stateR        the state of the rendition.
     * @param wInner        a string based on the first n-1 members of the n-ary Product being rendered.
     * @param wOuter        a string based on the last (nth) member of the n-ary Product being rendered.
     * @param attributeName the name of the last member (used internally to distinguish between attributes and elements).
     * @tparam R the Product type.
     * @return a String.
     */
    private def doNestedRender[R <: Product : ClassTag](format: Format, stateR: StateR, wInner: String, wOuter: String, attributeName: String): Try[String] = {
        // XXX: determine if attributeName corresponds to an optional attribute--Some(true), an attribute--Some(false), or a non-attribute: None.
        val maybeAttribute = attributeName match {
            case Extractor.optionalAttribute(_) => Some(true)
            case Extractor.attribute(_) => Some(false)
            case _ => None
        }
        // XXX: if maybeAttribute is defined, then isInternal will usually be true
        //      (the exception being for the last attribute of an all-attribute element).
        if (maybeAttribute.isDefined)
            stateR.addAttribute(wOuter)
        val sb = new mutable.StringBuilder()
        if (!stateR.isInternal) {
            sb.append(format.formatName(open = Some(true), stateR))
            sb.append(stateR.getAttributes)
            if (maybeAttribute.isEmpty) sb.append(format.formatName(open = None, stateR))
            else sb.append(" ")
        }
        if (maybeAttribute.isEmpty)
            sb.append(wInner)
        // CONSIDER appending format.delimiter
        if (maybeAttribute.isEmpty)
            sb.append(wOuter)
        if (!stateR.isInternal) {
            if (maybeAttribute.isDefined) sb.append(format.formatName(open = None, stateR))
            sb.append(format.formatName(open = Some(false), stateR))
        }
        Try(sb.toString())
    }

    private def doRenderSequence[R: Renderable : ClassTag](rs: Seq[R], format: Format, maybeName: Option[String]): Try[String] = {
        val separator = format.sequencer(None)
        val sb = new mutable.StringBuilder()
        sb.append(format.sequencer(Some(true)))
        var first = true
        for (r <- rs) {
            if (!first) sb.append(if (separator == "\n") format.newline else separator)
            val wy = TryUsing(StateR(maybeName)) { sr =>
                for {
                    rr <- tryNotNull(implicitly[Renderable[R]])(s"doRenderSequence: ${implicitly[ClassTag[R]]}")
                    result <- rr.render(r, format, sr)
                } yield result
            }
            wy match {
                case Success(w) => sb.append(w)
                case Failure(x) => Renderers.logger.warn("doRenderSequence: failure", x)
            }
            first = false
        }
        sb.append(format.newline)
        sb.append(format.sequencer(Some(false)))
        Success(sb.toString())
    }
}

object Renderers {

    val logger: Logger = LoggerFactory.getLogger(Renderers.getClass)

    private val cdata: Regex = """.*([<&>]).*""".r

    implicit val stringRenderer: Renderable[String] = Renderable {
        (t: String, _: Format, stateR: StateR) =>
            renderAttribute(
                t match {
                    case cdata(_) => s"""<![CDATA[$t]]>"""
                    case _ => t
                }, stateR.maybeName)
    } ^^ "stringRenderer"

    implicit val rendererOptionString: Renderable[Option[String]] = optionRenderer[String]// ^^ "rendererOptionString"

    implicit val intRenderer: Renderable[Int] = Renderable {
        (t: Int, _: Format, stateR: StateR) =>
            renderAttribute(t.toString, stateR.maybeName)
    } ^^ "intRenderer"

    implicit val rendererOptionInt: Renderable[Option[Int]] = optionRenderer[Int]// ^^ "rendererOptionInt"

    implicit val booleanRenderer: Renderable[Boolean] = Renderable { (t: Boolean, _: Format, stateR: StateR) =>
        renderAttribute(t.toString, stateR.maybeName)
    } ^^ "booleanRenderer"
    implicit val rendererOptionBoolean: Renderable[Option[Boolean]] = optionRenderer[Boolean]// ^^ "rendererOptionBoolean"

    implicit val doubleRenderer: Renderable[Double] = Renderable {
        (t: Double, _: Format, stateR: StateR) =>
            renderAttribute(t.toString, stateR.maybeName)
    } ^^ "doubleRenderer"

    implicit val longRenderer: Renderable[Long] = Renderable {
        (t: Long, _: Format, stateR: StateR) =>
            renderAttribute(t.toString, stateR.maybeName)
    } ^^ "longRenderer"

    private val renderers = new Renderers {}
    implicit val rendererText: Renderable[Text] = renderers.renderer1(Text)
    implicit val rendererOptionText: Renderable[Option[Text]] = renderers.optionRenderer[Text]

    def maybeAttributeName[R <: Product](r: R, index: Int, useName: Boolean = false): Option[String] =
        r.productElementName(index) match {
            case "$" => None
            case Extractor.optionalAttribute(x) => Some(x)
            case Extractor.attribute(x) => Some(x)
            case x => if (useName) Some(x) else None
        }

    private def renderAttribute(w: String, maybeName: Option[String]): Try[String] = Try {
        maybeName match {
            case Some(name) => s"""$name="$w""""
            case None => w
        }
    }
}

/**
 * Typeclass to specify the required behavior of an object that you want to render as a String.
 *
 * @tparam T the type of the object.
 */
trait Renderable[T] extends NamedFunction[Renderable[T]] {
    /**
     * This is the method which renders an object t of type T as a String, given three other parameters.
     *
     * @param t      the object to be rendered.
     * @param format the format to render the object in.
     * @param stateR the state of rendering.
     * @return a String representation of t.
     */
    def render(t: T, format: Format, stateR: StateR): Try[String]
}

object Renderable {
    val flog: Flog = Flog[Renderers]

    /**
     * Method which allows us to wrap a function as a Renderable.
     *
     * @param function the function to be wrapped. It has the same signature as this apply method so it is simply a syntactic convenience.
     * @tparam T the type to be rendered.
     * @return a Renderable[T].
     */
    def apply[T](function: (T, Format, StateR) => Try[String]): Renderable[T] = (t, f, s) => function(t, f, s)

    /**
     * Render the T value using the implicitly found Renderable[T].
     * Debug-log the renderer and its result.
     *
     * @param t      the value to be rendered.
     * @param format the Format.
     * @param stateR the StateR.
     * @tparam T the type of t.
     * @return a String.
     */
    def render[T: Renderable](t: T, format: Format, stateR: StateR): Try[String] =
        implicitly[Renderable[T]].render(t, format, stateR)
}

trait Format {
  def indent: Format

  val indents: Int

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String

  def delimiter: String = ", "

  def sequencer(open: Option[Boolean]): String

  def newline: String
}

abstract class BaseFormat(indents: Int) extends Format {
  val name: String

  private val tab = "  "

  def newline: String = "\n" + (tab * indents)

  protected def getClassName[T: ClassTag](stateR: StateR): String = stateR.maybeName.getOrElse(implicitly[ClassTag[T]].runtimeClass.getSimpleName)
}

case class FormatXML(indents: Int) extends BaseFormat(indents) {

  val name: String = "FormatXML"

  def indent: Format = copy(indents = indents + 1)

  override def delimiter: String = " "

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = {
    val name = getClassName(stateR)
    open match {
      case Some(true) => s"<$name"
      case Some(false) => s"</$name>"
      case None => ">"
    }
  }

  def sequencer(open: Option[Boolean]): String = newline
}

case class FormatText(indents: Int) extends BaseFormat(indents) {
  val name: String = "FormatText"

  def indent: Format = copy(indents = indents + 1)

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = {
    val name = getClassName(stateR)
    open match {
      case Some(true) => s"$name{"
      case Some(false) => "}"
      case None => ""
    }
  }

  def sequencer(open: Option[Boolean]): String = open match {
    case Some(true) => "["
    case Some(false) => "]"
    case None => "\n"
  }
}

// TESTME
case class FormatIndented(indents: Int) extends BaseFormat(indents) {
  val name: String = "FormatIndented"

  def indent: Format = copy(indents = indents + 1)

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = open match {
    case Some(true) => "{"
    case Some(false) => "}"
    case None => ""
  }

  def sequencer(open: Option[Boolean]): String = open match {
    case Some(true) => "["
    case Some(false) => "]"
    case None => ", "
  }
}

/**
 * Case class intended to take care of the state of rendering.
 * Rendering is complex for several reasons:
 * (1) a method such as render5 invokes render4, render3, etc. in order to process all of the members of a Product.
 * (2) attributes are special and need to be rendered within the opening tag of the top-level element.
 *
 * NOTE: attributes is mutable (it's a StringBuilder). It is retained as we do operations such as setName, recurse.
 * However, we must be careful to ensure that no attribute gets left behind.
 *
 * @param maybeName  an optional String.
 * @param attributes a (private) StringBuilder: accessible via addAttribute or getAttributes.
 * @param interior   false if we are at the top level of an element; false if we have been invoked from above.
 */
case class StateR(maybeName: Option[String], private val attributes: mutable.StringBuilder, interior: Boolean) extends AutoCloseable {

  def setName(name: String): StateR = maybeName match {
    case Some(_) => this
    case None => copy(maybeName = Some(name))
  }

  def recurse: StateR = copy(interior = true)

  def addAttribute(attrString: String): StateR = {
    attributes.append(" " + attrString)
    this
  }

  def getAttributes: String = {
    val result = attributes.toString()
    attributes.clear()
    result
  }

  def setName[R <: Product](r: R, index: Int): StateR = copy(maybeName = Renderers.maybeAttributeName(r, index, useName = true))

  def isInternal: Boolean = interior

  def close(): Unit = {
      if (attributes.toString().trim.nonEmpty)
          throw XmlException(s"StateR.close: attributes not empty: '$attributes'")
  }
}

object StateR {
  def apply(maybeName: Option[String]): StateR = new StateR(maybeName, new mutable.StringBuilder(""), interior = false)

  def apply(interior: Boolean): StateR = new StateR(None, new mutable.StringBuilder(""), interior)

  def apply(): StateR = apply(None)
}
