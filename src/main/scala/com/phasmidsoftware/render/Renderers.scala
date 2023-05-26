package com.phasmidsoftware.render

import com.phasmidsoftware.core.FP.tryNotNull
import com.phasmidsoftware.core.Utilities.sequence
import com.phasmidsoftware.core.{SmartBuffer, Text, TryUsing, XmlException}
import com.phasmidsoftware.kmldoc.KmlRenderers.optionRenderer
import com.phasmidsoftware.render.Renderer.renderAttribute
import com.phasmidsoftware.xml.{CDATA, Extractor}
import org.slf4j.{Logger, LoggerFactory}
import scala.annotation.unused
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Trait which defines generic and standard renderers.
 *
 */
trait Renderers {

//    val flog: Flog = Flog[Renderers]

    /**
     * Method to create a renderer fpr a case class with no members, or a case object.
     *
     * @tparam R the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer0[R <: Product : ClassTag]: Renderer[R] = Renderer {
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
     * @tparam B the (Renderer) type of the auxiliary object of type R.
     * @tparam R the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer0Super[B: Renderer, R <: Product : ClassTag](construct: B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) =>
            val b = lens(r)
            for {br <- tryNotNull(implicitly[Renderer[B]])("renderer0Super: Renderer[B]")
                 wInner <- br.render(b, format, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, "", r.productElementName(0))
                 } yield result
    } ^^ s"renderer0Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with one member.
     *
     * @param ignored (unused) a function which takes a P0 and yields an R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderer) type of the (single) member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer1[P0: Renderer, R <: Product : ClassTag](@unused ignored: P0 => R): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) =>
            for {wOuter <- renderOuter(r, r.productElement(0).asInstanceOf[P0], 0, format.indent)
                 result <- doNestedRender(format, stateR, "", wOuter, r.productElementName(0))
                 } yield result
    } ^^ s"renderer1: ${implicitly[ClassTag[R]]}"

    /**
     * Alternative method to create a renderer fpr a Product (e.g., case class) with one member but also an auxiliary object in a second parameter set.
     *
     * @param construct a function which takes a P0 and yields a function of B => R
     *                  (this is usually the apply method of a case class that has a second parameter set with one parameter of type B).
     * @tparam B  the (Renderer) type of the auxiliary object of type R.
     * @tparam P0 the (Renderer) type of the (single) member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer1Super[B: Renderer, P0: Renderer, R <: Product : ClassTag](construct: P0 => B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) =>
            val b = lens(r)
            val constructOuter: P0 => R = construct(_)(b)
            for {
                br <- tryNotNull(implicitly[Renderer[B]])("renderer1Super: Renderer[B]")
                // CONSIDER not invoking recurse here.
                wInner <- br.render(b, format, stateR.recurse)
                wOuter <- renderer1(constructOuter).render(r, format, stateR.recurse)
                result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
            } yield result
    } ^^ s"renderer1Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with two members.
     *
     * @param construct a function (P0, P1) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a function which takes an R, a Format, and a StateR as parameters and yields a Renderer[R].
     */
    def renderer2[P0: Renderer, P1: Renderer, R <: Product : ClassTag](construct: (P0, P1) => R): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) =>
            val objectOuter = r.productElement(1).asInstanceOf[P1]
            val constructorInner: P0 => R = construct(_, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0])
            for {wInner <- renderer1(constructorInner).render(objectInner, format, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 1, format.indent)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(1))
                 } yield result
    } ^^ s"renderer2: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with two members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderer) type of the auxiliary object of type R.
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer2Super[B: Renderer, P0: Renderer, P1: Renderer, R <: Product : ClassTag](construct: (P0, P1) => B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1) => R = construct(_, _)(b)
            for {br <- tryNotNull(implicitly[Renderer[B]])("renderer2Super: Renderer[B]")
                 wInner <- br.render(b, format, stateR.recurse)
                 wOuter <- renderer2(constructOuter).render(r, format, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
                 } yield result
        }
    } ^^ s"renderer2Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with three members.
     *
     * @param construct a function (P0, P1, P2) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer3[P0: Renderer, P1: Renderer, P2: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2) => R): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(2).asInstanceOf[P2]
            val constructorInner: (P0, P1) => R = construct(_, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1])
            for {wInner <- renderer2(constructorInner).render(objectInner, format, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 2, format.indent)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(2))
                 } yield result
        }
    } ^^ s"renderer3: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with three members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderer) type of the auxiliary object of type R.
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer3Super[B: Renderer, P0: Renderer, P1: Renderer, P2: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2) => B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2) => R = construct(_, _, _)(b)
            for {br <- tryNotNull(implicitly[Renderer[B]])("renderer3Super: Renderer[B]")
                 wInner <- br.render(b, format, stateR.recurse)
                 wOuter <- renderer3(constructOuter).render(r, format, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
                 } yield result
        }
    } ^^ s"renderer3Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with four members.
     *
     * @param construct a function (P0, P1, P2, P3) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam P3 the (Renderer) type of the fourth member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return Renderer[R].
     */
    def renderer4[P0: Renderer, P1: Renderer, P2: Renderer, P3: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2, P3) => R): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(3).asInstanceOf[P3]
            val constructorInner: (P0, P1, P2) => R = construct(_, _, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2])
            for {wInner <- renderer3(constructorInner).render(objectInner, format, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 3, format.indent)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(3))
                 } yield result
        }
    } ^^ s"renderer4: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with four members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2, P3) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderer) type of the auxiliary object of type R.
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam P3 the (Renderer) type of the fourth member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer4Super[B: Renderer, P0: Renderer, P1: Renderer, P2: Renderer, P3: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2, P3) => R = construct(_, _, _, _)(b)
            for {br <- tryNotNull(implicitly[Renderer[B]])("renderer4Super: Renderer[B]")
                 wInner <- br.render(b, format, stateR.recurse)
                 wOuter <- renderer4(constructOuter).render(r, format, stateR.recurse)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
                 } yield result
        }
    } ^^ s"renderer4Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with five members.
     *
     * @param construct a function (P0, P1, P2, P3, P4) => R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam P3 the (Renderer) type of the fourth member of Product type R.
     * @tparam P4 the (Renderer) type of the fifth member of Product type R.
     * @tparam R  the (Renderer) type of Renderer to be returned (must be a Product).
     * @return Renderer[R].
     */
    def renderer5[P0: Renderer, P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => R): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(4).asInstanceOf[P4]
            val constructorInner: (P0, P1, P2, P3) => R = construct(_, _, _, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2], r.productElement(3).asInstanceOf[P3])
            for {wInner <- renderer4(constructorInner).render(objectInner, format, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 4, format.indent)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(4))
                 } yield result
        }
    } ^^ s"renderer5: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with five members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2, P3, P4) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderer) type of the auxiliary object of type R.
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam P3 the (Renderer) type of the fourth member of Product type R.
     * @tparam P4 the (Renderer) type of the fifth member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer5Super[B: Renderer, P0: Renderer, P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2, P3, P4) => R = construct(_, _, _, _, _)(b)
            for {
                br <- tryNotNull(implicitly[Renderer[B]])("renderer5Super: Renderer[B]")
                wInner <- br.render(b, format, stateR.recurse)
                wOuter <- renderer5(constructOuter).render(r, format, stateR.recurse)
                result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
            } yield result
        }
    } ^^ s"renderer5Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with five members.
     *
     * @param construct a function which takes a P0, P1, P2, P3, P4 and yields an R (this is usually the apply method of a case class).
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam P3 the (Renderer) type of the fourth member of Product type R.
     * @tparam P4 the (Renderer) type of the fifth member of Product type R.
     * @tparam P5 the (Renderer) type of the sixth member of Product type R.
     * @tparam R  the (Renderer) type of Renderer to be returned (must be a Product).
     * @return Renderer[R].
     */
    def renderer6[P0: Renderer, P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => R): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val objectOuter = r.productElement(5).asInstanceOf[P5]
            val constructorInner: (P0, P1, P2, P3, P4) => R = construct(_, _, _, _, _, objectOuter)
            val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2], r.productElement(3).asInstanceOf[P3], r.productElement(4).asInstanceOf[P4])
            for {wInner <- renderer5(constructorInner).render(objectInner, format, stateR.recurse)
                 wOuter <- renderOuter(r, objectOuter, 4, format.indent)
                 result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(5))
                 } yield result
        }
    } ^^ s"renderer6: ${implicitly[ClassTag[R]]}"

    /**
     * Method to create a renderer fpr a Product (e.g., case class) with six members but also an auxiliary object in a second parameter set.
     *
     * @param construct a function (P0, P1, P2, P3, P4, P5) => R (this is usually the apply method of a case class).
     * @tparam B  the (Renderer) type of the auxiliary object of type R.
     * @tparam P0 the (Renderer) type of the first member of Product type R.
     * @tparam P1 the (Renderer) type of the second member of Product type R.
     * @tparam P2 the (Renderer) type of the third member of Product type R.
     * @tparam P3 the (Renderer) type of the fourth member of Product type R.
     * @tparam P4 the (Renderer) type of the fifth member of Product type R.
     * @tparam P5 the (Renderer) type of the sixth member of Product type R.
     * @tparam R  the type of Renderer to be returned (must be a Product).
     * @return a Renderer[R].
     */
    def renderer6Super[B: Renderer, P0: Renderer, P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => B => R)(lens: R => B): Renderer[R] = Renderer {
        (r: R, format: Format, stateR: StateR) => {
            val b = lens(r)
            val constructOuter: (P0, P1, P2, P3, P4, P5) => R = construct(_, _, _, _, _, _)(b)
            for {
                br <- tryNotNull(implicitly[Renderer[B]])("renderer5Super: Renderer[B]")
                wInner <- br.render(b, format, stateR.recurse)
                wOuter <- renderer6(constructOuter).render(r, format, stateR.recurse)
                result <- doNestedRender(format, stateR, wInner, wOuter, r.productElementName(0))
            } yield result
        }
    } ^^ s"renderer6Super: ${implicitly[ClassTag[R]]}"

    /**
     * Method to yield a renderer of Option[R].
     *
     * @tparam R the (Renderer) underlying type to be rendered.
     * @return a Renderer of Option[R].
     */
    def optionRenderer[R: Renderer : ClassTag]: Renderer[Option[R]] = Renderer {
        (ro: Option[R], format: Format, stateR: StateR) =>
            ro match {
                case Some(r) =>
                    val wo = stateR.maybeName match {
                        case Some(Extractor.optional(x)) => Some(x)
                        case Some(x) => Some(x)
                        case None => None
                    }
                    implicitly[Renderer[R]].render(r, format, StateR(wo))
                case None => Success("")
            }
    } ^^ s"optionRenderer: ${implicitly[ClassTag[R]]}"

    /**
     * Method to yield a Renderer[T] such that the rendering can be performed according to the renderables for one sub-type of T (R0).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @return a Renderer[T].
     */
    def rendererSuper1[T: ClassTag, R0 <: T : Renderer : ClassTag]: Renderer[T] = Renderer {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        renderable <- tryNotNull(implicitly[Renderer[R0]])(s"rendererSuper1: ${implicitly[ClassTag[T]]}")
                        result <- renderable.render(r, format, stateR)
                    } yield result
                case _ =>
                    Failure(XmlException(s"rendererSuper1: object of type ${t.getClass} is not a sub-type for ${implicitly[ClassTag[T]]}\n" +
                            s"Are you sure that, in the appropriate rendererSuperN definition, you've included all possible sub-types?"))
            }
    } ^^ s"rendererSuper1: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderer[T] such that the rendering can be performed according to the renderables for two sub-types of T (R0 or R1).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @return a Renderer[T].
     */
    def rendererSuper2[T: ClassTag, R0 <: T : Renderer : ClassTag, R1 <: T : Renderer : ClassTag]: Renderer[T] = Renderer {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        r0r <- tryNotNull(implicitly[Renderer[R0]])(s"rendererSuper2: ${implicitly[ClassTag[T]]}")
                        result <- r0r.render(r, format, stateR)
                    } yield result
                case _ => rendererSuper1[T, R1].render(t, format, stateR)
            }
    } ^^ s"rendererSuper2: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderer[T] such that the rendering can be performed according to the renderables for three sub-types of T (R0, R1, or R2).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @return a Renderer[T].
     */
    def rendererSuper3[T: ClassTag, R0 <: T : Renderer : ClassTag, R1 <: T : Renderer : ClassTag, R2 <: T : Renderer : ClassTag]: Renderer[T] = Renderer {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    val r0ry = tryNotNull(implicitly[Renderer[R0]])(s"rendererSuper3: ${implicitly[ClassTag[T]]}")
                    for (r0r <- r0ry; result <- r0r.render(r, format, stateR)) yield result
                case _ => rendererSuper2[T, R1, R2].render(t, format, stateR)
            }
    } ^^ s"rendererSuper3: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderer[T] such that the rendering can be performed according to the renderables for four sub-types of T (R0, R1, R2, or R3).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @tparam R3 another sub-type of T.
     * @return a Renderer[T].
     */
    def rendererSuper4[T: ClassTag, R0 <: T : Renderer : ClassTag, R1 <: T : Renderer : ClassTag, R2 <: T : Renderer : ClassTag, R3 <: T : Renderer : ClassTag]: Renderer[T] = Renderer {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        r0r <- tryNotNull(implicitly[Renderer[R0]])(s"rendererSuper4: ${implicitly[ClassTag[T]]}")
                        result <- r0r.render(r, format, stateR)
                    } yield result
                case _ => rendererSuper3[T, R1, R2, R3].render(t, format, stateR)
            }
    } ^^ s"rendererSuper4: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderer[T] such that the rendering can be performed according to the renderables for five sub-types of T (R0, R1, R2, R3, or R4).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @tparam R3 another sub-type of T.
     * @tparam R4 another sub-type of T.
     * @return a Renderer[T].
     */
    def rendererSuper5[T: ClassTag, R0 <: T : Renderer : ClassTag, R1 <: T : Renderer : ClassTag, R2 <: T : Renderer : ClassTag, R3 <: T : Renderer : ClassTag, R4 <: T : Renderer : ClassTag]: Renderer[T] = Renderer {
        (t: T, format: Format, stateR: StateR) =>
            t match {
                case r: R0 =>
                    for {
                        r0r <- tryNotNull(implicitly[Renderer[R0]])(s"rendererSuper5: ${implicitly[ClassTag[T]]}")
                        result <- r0r.render(r, format, stateR)
                    } yield result
                case _ => rendererSuper4[T, R1, R2, R3, R4].render(t, format, stateR)
            }
    } ^^ s"rendererSuper5: ${implicitly[ClassTag[T]]}"

    /**
     * Method to yield a Renderer[T] such that the rendering can be performed according to the renderables for six sub-types of T (R0, R1, R2, R3, R4, or R5).
     *
     * @tparam T  the super-type and the underlying type of the result.
     * @tparam R0 one sub-type of T.
     * @tparam R1 another sub-type of T.
     * @tparam R2 another sub-type of T.
     * @tparam R3 another sub-type of T.
     * @tparam R4 another sub-type of T.
     * @tparam R5 another sub-type of T.
     * @return a Renderer[T].
     */
    def rendererSuper6[T: ClassTag, R0 <: T : Renderer : ClassTag, R1 <: T : Renderer : ClassTag, R2 <: T : Renderer : ClassTag, R3 <: T : Renderer : ClassTag, R4 <: T : Renderer : ClassTag, R5 <: T : Renderer : ClassTag]: Renderer[T] = Renderer { (t: T, format: Format, stateR: StateR) =>
        t match {
            case r: R0 =>
                for {
                    r0r <- tryNotNull(implicitly[Renderer[R0]])(s"rendererSuper6: ${implicitly[ClassTag[T]]}")
                    result <- r0r.render(r, format, stateR)
                } yield result
            case _ => rendererSuper5[T, R1, R2, R3, R4, R5].render(t, format, stateR)
        }
    } ^^ s"rendererSuper6: ${implicitly[ClassTag[T]]}"

    /**
     * Method to return a Renderer of Seq[R].
     *
     * @tparam R the underlying element type.
     * @return a Renderer of Seq[R].
     */
    def sequenceRenderer[R: Renderer]: Renderer[Seq[R]] = Renderer {
        (rs: Seq[R], format: Format, _: StateR) => doRenderSequence(rs, format, None)
    }

    /**
     * Method to return a Renderer of Seq[R] with a pre-defined format.
     *
     * NOTE This is required for allowing a format to take precedence over the format parameter passed into the render method.
     *
     * @param formatFunc a function which, given the current state of indents, yields the required format to pass into doRenderSequence.
     * @tparam R the underlying type to be rendered.
     * @return a Renderer of Seq[R].
     */
    def sequenceRendererFormatted[R: Renderer : ClassTag](formatFunc: Int => Format): Renderer[Seq[R]] = Renderer {
        (rs: Seq[R], format: Format, stateR: StateR) =>
            doRenderSequence(rs, formatFunc(format.indents), stateR.maybeName)
    } ^^ s"sequenceRendererFormatted: ${implicitly[ClassTag[R]]}"

    // CONSIDER Moving to Renderer
    private def renderOuter[R <: Product : ClassTag, P: Renderer](r: R, objectOuter: P, indexOuter: Int, format: Format): Try[String] =
        TryUsing(StateR().setName(r, indexOuter)) {
            sr =>
                for {p <- tryNotNull(implicitly[Renderer[P]])(s"renderOuter: ${implicitly[ClassTag[R]]}")
                     x <- p.render(objectOuter, format, sr)
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
     * CONSIDER Moving this into Renderer.
     *
     * @param formatIn      the format in which the Product should be rendered.
     * @param stateR        the state of the rendition.
     * @param wInner        a string based on the first n-1 members of the n-ary Product being rendered.
     * @param wOuter        a string based on the last (nth) member of the n-ary Product being rendered.
     * @param attributeName the name of the last member (used internally to distinguish between attributes and elements).
     * @tparam R the Product type.
     * @return a String.
     */
    private def doNestedRender[R <: Product : ClassTag](formatIn: Format, stateR: StateR, wInner: String, wOuter: String, attributeName: String): Try[String] = {
        // XXX: determine if attributeName corresponds to an optional attribute--Some(true), an attribute--Some(false), or a non-attribute: None.
        val maybeAttribute = Extractor.inferAttributeType(attributeName)
        val format = if (maybeAttribute.isDefined || attributeName == "$" || attributeName == "") formatIn.flatten else formatIn
        // XXX: if maybeAttribute is defined, then isInternal will usually be true
        //      (the exception being for the last attribute of an all-attribute element).
        if (maybeAttribute.isDefined)
            stateR.addAttribute(wOuter)
        val sb = SmartBuffer()
        if (!stateR.isInternal) {
            sb.appendPadded(format.formatName(open = Some(true), stateR))
            sb.appendPadded(stateR.getAttributes)
            if (maybeAttribute.isEmpty) sb.append(format.formatName(open = None, stateR))
        }
        if (maybeAttribute.isEmpty) {
            sb.appendPadded(wInner)
            sb.appendPadded(wOuter)
        }
        if (!stateR.isInternal) {
            if (maybeAttribute.isDefined) sb.append(format.formatName(open = None, stateR))
            sb.append(format.formatName(open = Some(false), stateR))
        }
        Try(sb.result)
    }

    private def doRenderSequence[R: Renderer](rs: Seq[R], format: Format, maybeName: Option[String]): Try[String] = {
        val formatIndented = format.indent
        val separator = format.sequencer(None)
        val sep = if (separator == "\n") format.newline else separator

        def trim(sy: Try[String]): Try[String] = sy map (_.trim)

        def formatSequence(ws: Seq[String]) = ws.mkString(formatIndented.sequencer(Some(true)), sep, format.sequencer(Some(false)))

        // If the separator is newline, and if the elements generated from rs is non-empty then we trim all elements except for the first.
        val elements = (separator, rs map (renderR(_)(format, StateR(maybeName)))) match {
            case ("\n", h :: t) => h :: (t map trim)
            case (_, x) => x
        }
        sequence(elements) map formatSequence
    }

    private def renderR[R: Renderer](r: R)(format: Format, stateR: StateR) = TryUsing(stateR) {
        sr =>
            for {
                rr <- tryNotNull(implicitly[Renderer[R]])(s"doRenderSequence")
                result <- rr.render(r, format, sr)
            } yield result
    }
}

object Renderers {

    val logger: Logger = LoggerFactory.getLogger(Renderers.getClass)

    implicit val charSequenceRenderer: Renderer[CharSequence] = Renderer {
        (x: CharSequence, _: Format, stateR: StateR) =>
            renderAttribute(
                x match {
                    case CDATA(t, pre, post) => s"""$pre<![CDATA[$t]]>$post"""
                    case _ => x.toString
                }, stateR.maybeName)
    } ^^ "charSequenceRenderer"

    implicit val stringRenderer: Renderer[String] = Renderer {
        (x: String, _: Format, stateR: StateR) =>
            renderAttribute(x, stateR.maybeName)
    }

    // CONSIDER why do we not get this from Renderers
    implicit val rendererOptionString: Renderer[Option[String]] = optionRenderer[String] ^^ "rendererOptionString"

    implicit val rendererSequenceString: Renderer[Seq[String]] = new Renderers {}.sequenceRenderer[String] ^^ "rendererSequenceString"

    implicit val intRenderer: Renderer[Int] = Renderer {
        (t: Int, _: Format, stateR: StateR) => renderAttribute(t.toString, stateR.maybeName)
    } ^^ "intRenderer"
    implicit val rendererOptionInt: Renderer[Option[Int]] = optionRenderer[Int]// ^^ "rendererOptionInt"

    implicit val booleanRenderer: Renderer[Boolean] = Renderer {
        (t: Boolean, _: Format, stateR: StateR) => renderAttribute(t.toString, stateR.maybeName)
    } ^^ "booleanRenderer"
    implicit val rendererOptionBoolean: Renderer[Option[Boolean]] = optionRenderer[Boolean]// ^^ "rendererOptionBoolean"

    implicit val doubleRenderer: Renderer[Double] = Renderer {
        (t: Double, _: Format, stateR: StateR) =>
            val sb = new StringBuilder(t.toString)
            while (sb.endsWith("0")) sb.setLength(sb.length() - 1)
            if (sb.endsWith(".")) sb.setLength(sb.length() - 1)
            renderAttribute(sb.toString, stateR.maybeName)
    } ^^ "doubleRenderer"
    implicit val rendererOptionDouble: Renderer[Option[Double]] = optionRenderer[Double]

    implicit val longRenderer: Renderer[Long] = Renderer {
        (t: Long, _: Format, stateR: StateR) => renderAttribute(t.toString, stateR.maybeName)
    } ^^ "longRenderer"

    private val renderers = new Renderers {}
    implicit val rendererText: Renderer[Text] = renderers.renderer1(Text.apply)
    implicit val rendererOptionText: Renderer[Option[Text]] = renderers.optionRenderer[Text]
}
