package com.phasmidsoftware.xml

import com.phasmidsoftware.core.FP.tryNotNull
import com.phasmidsoftware.core.Utilities.sequence
import com.phasmidsoftware.core.{Reflection, XmlException}
import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.xml.Extractor.{expandTranslations, extractChildren, extractElementsByLabel, extractField, none}
import com.phasmidsoftware.xml.Extractors.{MultiExtractorBase, extractSequence, fieldNamesMaybeDropLast}

import scala.Function.uncurried
import scala.reflect.{ClassTag, classTag}
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait which defines many useful Extractors, where the result is an instance of Extractor[T].
 */
trait Extractors {

  /**
   * Method to yield an Extractor of Option[P] where there is evidence of Extractor[P].
   * This is generally used in conjunction with naming a case class member as "maybe"name.
   *
   * CONSIDER re-writing this as it appear to force evaluation of the implicit evidence of Extractor[P].
   *
   * @tparam P the underlying type of the result.
   * @return an Extractor of Option[P].
   */
  def extractorOption[P: Extractor]: Extractor[Option[P]] = implicitly[Extractor[P]] map (p => Option(p))

    /**
     * Method to yield an Extractor of a super-type based on the extractor of a sub-type.
     *
     * @tparam R  result type.
     * @tparam P0 extractor type.
     * @return an Extractor[R].
     */
    def extractorSubtype[R, P0 <: R : Extractor]: Extractor[R] = none[R].|[P0]()(implicitly[Extractor[P0]])

    /**
   * Method to yield an Extractor which can choose from (two) alternative extractors.
   *
   * FIXME this doesn't work properly: the order of P0 and P1 is significant: it shouldn't be.
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @return an Extractor[R].
   */
  def extractorAlt[R, P0 <: R : Extractor, P1 <: R : Extractor]: Extractor[R] = none[R].|[P0]()(implicitly[Extractor[P0]]).|[P1]()(implicitly[Extractor[P1]])

  /**
   * Method to yield an Extractor which can choose from three other extractors.
   * Why is this method not called extractorAlt3? Because I know my Latin ;)
   *
   * TODO remove explicit implicit parameters
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @tparam P2 third extractor type.
   * @return an Extractor[R].
   */
  def extractorAlia3[R, P0 <: R : Extractor, P1 <: R : Extractor, P2 <: R : Extractor]: Extractor[R] = none[R].|[P0]()(implicitly[Extractor[P0]]).|[P1]()(implicitly[Extractor[P1]]).|[P2]()(implicitly[Extractor[P2]])

  /**
   * Method to yield an Extractor which can choose from four other extractors.
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @tparam P2 third extractor type.
   * @tparam P3 fourth extractor type.
   * @return an Extractor[R].
   */
  def extractorAlia4[R, P0 <: R : Extractor, P1 <: R : Extractor, P2 <: R : Extractor, P3 <: R : Extractor]: Extractor[R] = none[R].|[P0]().|[P1]().|[P2]().|[P3]()

  /**
   * Method to yield an Extractor which can choose from five other extractors.
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @tparam P2 third extractor type.
   * @tparam P3 fourth extractor type.
   * @tparam P4 fifth extractor type.
   * @return an Extractor[R].
   */
  def extractorAlia5[R, P0 <: R : Extractor, P1 <: R : Extractor, P2 <: R : Extractor, P3 <: R : Extractor, P4 <: R : Extractor]: Extractor[R] = none[R].|[P0]().|[P1]().|[P2]().|[P3]().|[P4]()

  /**
   * Method to yield an Extractor which can choose from six other extractors.
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @tparam P2 third extractor type.
   * @tparam P3 fourth extractor type.
   * @tparam P4 fifth extractor type.
   * @tparam P5 sixth extractor type.
   * @return an Extractor[R].
   */
  def extractorAlia6[R, P0 <: R : Extractor, P1 <: R : Extractor, P2 <: R : Extractor, P3 <: R : Extractor, P4 <: R : Extractor, P5 <: R : Extractor]: Extractor[R] = none[R].|[P0]()(implicitly[Extractor[P0]]).|[P1]()(implicitly[Extractor[P1]]).|[P2]()(implicitly[Extractor[P2]]).|[P3]()(implicitly[Extractor[P3]]).|[P4]()(implicitly[Extractor[P4]]).|[P5]()(implicitly[Extractor[P5]])

    /**
     * Extractor which will convert an Xml Node into a sequence of P objects where there is evidence of Extractor[P].
     *
     * NOTE This method is used to extract an iterable from a Node, whereas we use MultiExtractor to extract an iterable from a NodeSeq.
     *
     * @param label the label of the child nodes to be returned.
     * @tparam P the underlying type of the result.
     * @return an Extractor of Iterable[P].
     */
    def extractorIterable[P: Extractor](label: String): Extractor[Iterable[P]] = (node: Node) => extractSequence[P](node / label)

    /**
     * Method to create a new MultiExtractor based on type P such that the underlying type of the result
     * is Seq[P].
     *
     * @tparam P the underlying (Extractor) type.
     * @return a MultiExtractor of Seq[P]
     */
    def multiExtractorBase[P: Extractor]: MultiExtractor[Seq[P]] = new MultiExtractorBase[P]()

    def multiExtractor[P](f: NodeSeq => Try[Seq[P]]): MultiExtractor[Seq[P]] = (nodeSeq: NodeSeq) => f(nodeSeq)

    /**
     * Method to yield an ElementExtractor[P] which in turns invokes extractField with the given tag.
     *
     * @tparam P the underlying (Extractor) type of the result.
     * @return an ElementExtractor[P] based on extractField.
     */
    def fieldExtractor[P: Extractor]: ElementExtractor[P] = (tag: String) => extractField[P](tag)

    /**
     * Method to yield an ElementExtractor[P] which in turns invokes extractChildren with the given tag.
     *
     * @tparam P the underlying (MultiExtractor) type of the result.
     * @return an ElementExtractor[P] based on extractField.
     */
    def childrenExtractor[P: MultiExtractor]: ElementExtractor[P] = (tag: String) => extractChildren[P](tag)

    /**
     * Method to yield a MultiExtractor of Seq[T] such that T is the super-type of P0.
     *
     * FIXME: this logic seems to be deeply flawed: expanding the labels means that the types and labels may no longer be aligned.
     *
     * @param construct a function whose sole purpose is to enable type inference (construct is never referenced in the code).
     * @param labels    the label of the elements we wish to extract (wrapped in Seq). The one label must correspond to P0.
     * @tparam T  the ultimate underlying type of the resulting MultiExtractor.
     * @tparam U  a tuple whose only purpose is type inference.
     * @tparam P0 the first (Extractor) sub-type of T.
     * @return MultiExtractor of Seq[T].
     */
    def multiExtractor1[T, U <: Product, P0 <: T : Extractor: ClassTag](construct: P0 => U, labels: Seq[String]): MultiExtractor[Seq[T]] = nodeSeq =>
        expandTranslations(labels) match {
            case label :: Nil =>
                sequence(extractElementsByLabel[P0](nodeSeq, label))
            case fs => Failure(XmlException(s"multiExtractor1: logic error for labels: $fs")) // TESTME
        }


    /**
     * Method to yield a MultiExtractor of Seq[T] such that T is the super-type of two P-types.
     *
     * @param construct a function whose sole purpose is to enable type inference (construct is never referenced in the code).
     * @param labels    the labels of the elements we wish to extract. These must be in the same sequence as the corresponding P-types.
     * @tparam T  the ultimate underlying type of the resulting MultiExtractor.
     * @tparam U  a tuple whose only purpose is type inference.
     * @tparam P0 the first (Extractor) sub-type of T.
     * @tparam P1 the second (Extractor) sub-type of T.
     * @return MultiExtractor of Seq[T].
     */
//    def multiExtractor2[T, U <: Product, P0 <: T, P1 <: T](construct: (P0, P1) => U, labels: Seq[String])(implicit evp0: Extractor[P0], evp1: Extractor[P1], evcp0: ClassTag[P0], evcp1: ClassTag[P1]): MultiExtractor[Seq[T]] = nodeSeq =>
    def multiExtractor2[T: ClassTag, U <: Product, P0 <: T : Extractor: ClassTag, P1 <: T : Extractor: ClassTag](construct: (P0, P1) => U, labels: Seq[String]): MultiExtractor[Seq[T]] =
    multiExtractor[T]{
        nodeSeq =>
            println(s"multiExtractor2: ${implicitly[ClassTag[T]]} from ${implicitly[ClassTag[P0]]}, ${implicitly[ClassTag[P1]]} with labels: $labels")

            val extractors = new Extractors{}
            println(extractors)
    expandTranslations(labels) match {
        case label :: fs =>
            val p0sy = sequence(extractElementsByLabel[P0](nodeSeq, label)(implicitly[Extractor[P0]]))
            val tsy: Try[Seq[T]] = multiExtractor1[T, Tuple1[P1], P1](p1 => Tuple1(p1), fs)(implicitly[Extractor[P1]],classTag).extract(nodeSeq)
            for (ts1 <- tsy; ts2 <- p0sy) yield ts1 ++ ts2
    }
    }

    /**
     * Method to yield a MultiExtractor of Seq[T] such that T is the super-type of three P-types.
     * The labels are "translated" according to the translation table, which may produce more labels than in the original.
     * The first label is used to filter the P0-type child nodes of nodeSeq (the input of the resulting MultiExtractor).
     * Then, the resulting
     *
     * @param construct a function whose sole purpose is to enable type inference (construct is never referenced in the code).
     * @param labels    the labels of the elements we wish to extract. These must be in the same sequence as the corresponding P-types.
     * @tparam T  the ultimate underlying type of the resulting MultiExtractor.
     * @tparam U  a tuple whose only purpose is type inference.
     * @tparam P0 the first (Extractor) sub-type of T.
     * @tparam P1 the second (Extractor) sub-type of T.
     * @tparam P2 the third (Extractor) sub-type of T.
     * @return MultiExtractor of Seq[T].
     */
    def multiExtractor3[T: ClassTag, U <: Product, P0 <: T : Extractor: ClassTag, P1 <: T : Extractor: ClassTag, P2 <: T : Extractor: ClassTag](construct: (P0, P1, P2) => U, labels: Seq[String]): MultiExtractor[Seq[T]] = nodeSeq =>
        expandTranslations(labels) match {
            case label :: fs =>
                val p0sy = sequence(extractElementsByLabel[P0](nodeSeq, label))
                val tsy = multiExtractor2[T, (P1, P2), P1, P2]((p1, p2) => (p1, p2), fs).extract(nodeSeq)
                for (ts1 <- tsy; ts2 <- p0sy) yield ts1 ++ ts2
        }

    /**
     * Method to yield a MultiExtractor of Seq[T] such that T is the super-type of four P-types.
     *
     * @param construct a function whose sole purpose is to enable type inference (construct is never referenced in the code).
     * @param labels    the labels of the elements we wish to extract. These must be in the same sequence as the corresponding P-types.
     * @tparam T  the ultimate underlying type of the resulting MultiExtractor.
     * @tparam U  a tuple whose only purpose is type inference.
     * @tparam P0 the first (Extractor) sub-type of T.
     * @tparam P1 the second (Extractor) sub-type of T.
     * @tparam P2 the third (Extractor) sub-type of T.
     * @tparam P3 the fourth (Extractor) sub-type of T.
     * @return MultiExtractor of Seq[T].
     */
    def multiExtractor4[T: ClassTag, U <: Product, P0 <: T : Extractor: ClassTag, P1 <: T : Extractor: ClassTag, P2 <: T : Extractor: ClassTag, P3 <: T : Extractor: ClassTag](construct: (P0, P1, P2, P3) => U, labels: Seq[String]): MultiExtractor[Seq[T]] = nodeSeq =>
        expandTranslations(labels) match {
            case label :: fs =>
                val p0sy = sequence(extractElementsByLabel[P0](nodeSeq, label))
                val tsy = multiExtractor3[T, (P1, P2, P3), P1, P2, P3]((p1, p2, p3) => (p1, p2, p3), fs).extract(nodeSeq)
                for (ts1 <- tsy; ts2 <- p0sy) yield ts1 ++ ts2
        }

    /**
     * Method to yield a MultiExtractor of Seq[T] such that T is the super-type of five P-types.
     *
     * @param construct a function whose sole purpose is to enable type inference (construct is never referenced in the code).
     * @param labels    the labels of the elements we wish to extract. These must be in the same sequence as the corresponding P-types.
     * @tparam T  the ultimate underlying type of the resulting MultiExtractor.
     * @tparam U  a tuple whose only purpose is type inference.
     * @tparam P0 the first (Extractor) sub-type of T.
     * @tparam P1 the second (Extractor) sub-type of T.
     * @tparam P2 the third (Extractor) sub-type of T.
     * @tparam P3 the fourth (Extractor) sub-type of T.
     * @tparam P4 the fifth (Extractor) sub-type of T.
     * @return MultiExtractor of Seq[T].
     */
    def multiExtractor5[T: ClassTag, U <: Product, P0 <: T : Extractor: ClassTag, P1 <: T : Extractor: ClassTag, P2 <: T : Extractor: ClassTag, P3 <: T : Extractor: ClassTag, P4 <: T : Extractor: ClassTag](construct: (P0, P1, P2, P3, P4) => U, labels: Seq[String]): MultiExtractor[Seq[T]] = nodeSeq =>
        expandTranslations(labels) match {
            case label :: fs =>
                val p0sy = sequence(extractElementsByLabel[P0](nodeSeq, label))
                val tsy = multiExtractor4[T, (P1, P2, P3, P4), P1, P2, P3, P4]((p1, p2, p3, p4) => (p1, p2, p3, p4), fs).extract(nodeSeq)
                for (ts1 <- tsy; ts2 <- p0sy) yield ts1 ++ ts2
        }

    /**
     * Method to yield a MultiExtractor of Seq[T] such that T is the super-type of six P-types.
     *
     * @param construct a function whose sole purpose is to enable type inference (construct is never referenced in the code).
     * @param labels    the labels of the elements we wish to extract. These must be in the same sequence as the corresponding P-types.
     * @tparam T  the ultimate underlying type of the resulting MultiExtractor.
     * @tparam U  a tuple whose only purpose is type inference.
     * @tparam P0 the first (Extractor) sub-type of T.
     * @tparam P1 the second (Extractor) sub-type of T.
     * @tparam P2 the third (Extractor) sub-type of T.
     * @tparam P3 the fourth (Extractor) sub-type of T.
     * @tparam P4 the fifth (Extractor) sub-type of T.
     * @tparam P5 the sixth (Extractor) sub-type of T.
     * @return MultiExtractor of Seq[T].
     */
    def multiExtractor6[T:ClassTag, U <: Product, P0 <: T : Extractor: ClassTag, P1 <: T : Extractor: ClassTag, P2 <: T : Extractor: ClassTag, P3 <: T : Extractor: ClassTag, P4 <: T : Extractor: ClassTag, P5 <: T : Extractor: ClassTag](construct: (P0, P1, P2, P3, P4, P5) => U, labels: Seq[String]): MultiExtractor[Seq[T]] = nodeSeq =>
        expandTranslations(labels) match {
            case label :: fs =>
                val p0sy = sequence(extractElementsByLabel[P0](nodeSeq, label))
                val tsy = multiExtractor5[T, (P1, P2, P3, P4, P5), P1, P2, P3, P4, P5]((p1, p2, p3, p4, p5) => (p1, p2, p3, p4, p5), fs).extract(nodeSeq)
                for (ts1 <- tsy; ts2 <- p0sy) yield ts1 ++ ts2
        }

    /**
     * Method to yield an Extractor[T] where we have an Extractor[B => T].
     * This will occur when we have a case class with an additional parameter set
     * including one parameter of type B.
     *
     * CONSIDER rename this (compose? or extractorCompose?) and couldn't it go inside Extractor?
     *
     * CONSIDER inverting the parametric types so that the resulting type (T) is first.
     *
     * @param extractorBtoT an extractor for the type B => T.
     * @tparam B the type of the value in the additional parameter set of T.
     * @tparam T the underlying type of the resulting extractor.
     * @return an Extractor[T] whose method extract will convert a Node into a T.
     */
    def extractorPartial[B <: Product : Extractor, T: ClassTag](extractorBtoT: Extractor[B => T]): Extractor[T] =
        Extractor { (node: Node) =>
            for {
                b2te <- tryNotNull(extractorBtoT)(s"extractorPartial: extractorBtoT where T is ${implicitly[ClassTag[T]]}")
                 be <- tryNotNull(implicitly[Extractor[B]])(s"extractorPartial: extractorB")
                 q <- b2te.extract(node)
                 b <- be.extract(node)
                 } yield q(b)
        } //^^ combineNameds2[Extractor[B], Extractor[B => T]](extractorBtoT)

    /**
     * Extractor which will convert an Xml Node (which is ignored) into an instance of a case object or case class.
     * NOTE that you will have to specify a lambda for the construct function.
     *
     * @param construct a function () => T, usually the apply method of a case object or zero-member case class.
     * @tparam T the underlying type of the result, a Product.
     * @return an Extractor[T] whose method extract will construct a T while ignoring the input Node.
     */
    def extractor0[T: ClassTag](construct: Unit => T): Extractor[T] = Extractor(Success(construct())) ^^ "extractor0"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with zero members and one auxiliary (non-member) parameter.
     *
     * NOTE: the construct function will have to be an explicitly declared function of the form: B => T.
     * This is because the compiler gets a little confused, otherwise.
     *
     * @param construct a function B => T, an explicitly declared function.
     * @tparam B the type of the non-member parameter of T.
     * @tparam T the underlying type of the result, a Product.
     * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
     */
    def extractorPartial0[B, T <: Product : ClassTag](construct: B => T): Extractor[B => T] = Extractor(Success(construct)) ^^ "extractorPartial0"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with one member.
     *
     * NOTE: this specific extractor provides logging.
     * CONSIDER adding logging to the other extractors.
     *
     * @param construct a function (P0) => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first (only) member of the Product type T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
     */
    def extractor10[P0: Extractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] = Extractor {
        import Extractors.flog._
        (node: Node) => "extractor10: " |! (extractorPartial1[P0, Unit, T](fieldExtractor, e0 => _ => construct(e0), dropLast = false, fields).extract(node) map (z => z()))
    } //^^ s"extractor10(${name[Extractor[P0]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with one member.
     *
     * @param construct a function (P0) => T, usually the apply method of a case class.
     * @tparam P0 the (MultiExtractor) type of the first (only) member of the Product type T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
     */
    def extractor01[P0: MultiExtractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] = Extractor {
        (node: Node) => extractorPartial1[P0, Unit, T](childrenExtractor[P0], m0 => _ => construct(m0), dropLast = false, fields).extract(node) map (z => z())
    } //^^ s"extractor01(${name[MultiExtractor[P0]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with one member and one auxiliary (non-member) parameter.
     *
     * @param construct a function (P0) => B => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first (only) member of the Product type T.
     * @tparam B  the type of the non-member parameter of T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
     */
    def extractorPartial10[P0: Extractor, B, T <: Product : ClassTag](construct: P0 => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
        Extractor((node: Node) => extractorPartial1[P0, B, T](fieldExtractor, e0 => b => construct(e0)(b), dropLast = true, fields).extract(node)) //^^ s"extractorPartial10(${name[Extractor[P0]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with one member and one auxiliary (non-member) parameter.
     *
     * @param construct a function (P0) => B => T, usually the apply method of a case class.
     * @tparam P0 the (MultiExtractor) type of the first (only) member of the Product type T.
     * @tparam B  the type of the non-member parameter of T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
     */
    def extractorPartial01[P0: MultiExtractor, B, T <: Product : ClassTag](construct: P0 => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
        Extractor((node: Node) => extractorPartial1[P0, B, T](childrenExtractor, m0 => b => construct(m0)(b), dropLast = true, fields).extract(node))// ^^ s"extractorPartial01(${name[MultiExtractor[P0]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with two members.
     *
     * @param construct a function (P0,P1) => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first member of the Product type T.
     * @tparam P1 the (Extractor) type of the second member of the Product type T.
     * @tparam T  the underlying type of the result, a Product with two members.
     * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
     */
    def extractor20[P0: Extractor, P1: Extractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
        Extractor((node: Node) => extractorPartial2[P0, P1, Unit, T](fieldExtractor[P0], extractorPartial10[P1, Unit, T](_, _), (e0, e1) => _ => construct(e0, e1), dropLast = false, fields).extract(node) map (z => z()))// ^^
//                s"extractor20(${combineNamed2[Extractor[P0], Extractor[P1]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with two members.
     *
     * @param construct a function (P0,P1) => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first member of the Product type T.
     * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
     */
    def extractor11[P0: Extractor, P1: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
        Extractor((node: Node) => extractorPartial2[P0, P1, Unit, T](fieldExtractor[P0], extractorPartial01[P1, Unit, T](_, _), (e0, e1) => _ => construct(e0, e1), dropLast = false, fields).extract(node) map (z => z()))// ^^
//                s"extractor11(${combineNamed2[Extractor[P0], MultiExtractor[P1]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with two members.
     *
     * TESTME
     *
     * @param construct a function (P0,P1) => T, usually the apply method of a case class.
     * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
     * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
     */
    def extractor02[P0: MultiExtractor, P1: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
        Extractor((node: Node) => extractorPartial2[P0, P1, Unit, T](childrenExtractor[P0], extractorPartial01[P1, Unit, T](_, _), (m0, m1) => _ => construct(m0, m1), dropLast = false, fields).extract(node) map (z => z()))// ^^
//                s"extractor02(${combineNamed2[MultiExtractor[P0], MultiExtractor[P1]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with two members and one auxiliary (non-member) parameter.
     *
     * @param construct a function (P0, P1) => B => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first member of the Product type T.
     * @tparam P1 the (Extractor) type of the second member of the Product type T.
     * @tparam B  the type of the non-member parameter of T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
     */
    def extractorPartial20[P0: Extractor, P1: Extractor, B, T <: Product : ClassTag](construct: (P0, P1) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
        Extractor((node: Node) => extractorPartial2[P0, P1, B, T](fieldExtractor[P0], extractorPartial10[P1, B, T](_, _), construct, dropLast = true, fields).extract(node))// ^^
//                s"extractorPartial20(${combineNamed2[Extractor[P0], Extractor[P1]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with two members and one auxiliary (non-member) parameter.
     *
     * @param construct a function (P0, P1) => B => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first member of the Product type T.
     * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
     * @tparam B  the type of the non-member parameter of T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
     */
    def extractorPartial11[P0: Extractor, P1: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
        Extractor((node: Node) => extractorPartial2[P0, P1, B, T](fieldExtractor[P0], extractorPartial01[P1, B, T](_, _), construct, dropLast = true, fields).extract(node))// ^^
//                s"extractorPartial11(${combineNamed2[Extractor[P0], MultiExtractor[P1]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with two members and one auxiliary (non-member) parameter.
     *
     * @param construct a function (P1, P2) => B => T, usually the apply method of a case class.
     * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
     * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
     * @tparam B  the type of the non-member parameter of T.
     * @tparam T  the underlying type of the result, a Product.
     * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
     */
    def extractorPartial02[P0: MultiExtractor, P1: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
        Extractor((node: Node) => extractorPartial2[P0, P1, B, T](childrenExtractor[P0], extractorPartial01[P1, B, T](_, _), construct, dropLast = true, fields).extract(node))// ^^
//                s"extractorPartial02(${combineNamed2[MultiExtractor[P0], MultiExtractor[P1]]})"

    /**
     * Extractor which will convert an Xml Node into an instance of a case class with three members.
     *
     * CONSIDER re-implement all similar extractors using extractorPartial30, etc.
     * This can be done but requires turning dropLast to false when invoking extractorPartial30 (which doesn't currently have such a parameter).
     * See the commented code for how this should be done (but with the change mentioned).
     *
     * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
     * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor30[P0: Extractor, P1: Extractor, P2: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](fieldExtractor[P0], extractorPartial20[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())
//  XXX   (node: Node) => extractorPartial30[P0,P1,P2,Unit,T]((e0, e1, e2) => _ => construct(e0, e1, e2), fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor21[P0: Extractor, P1: Extractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](fieldExtractor[P0], extractorPartial11[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor12[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](fieldExtractor[P0], extractorPartial02[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor03[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](childrenExtractor[P0], extractorPartial02[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial30[P0: Extractor, P1: Extractor, P2: Extractor, B, T <: Product : ClassTag](construct: (P0, P1, P2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](fieldExtractor[P0], extractorPartial20[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial21[P0: Extractor, P1: Extractor, P2: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](fieldExtractor[P0], extractorPartial11[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial12[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](fieldExtractor[P0], extractorPartial02[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2) => B => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial03[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](childrenExtractor[P0], extractorPartial02[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor40[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](fieldExtractor[P0], extractorPartial30[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor31[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](fieldExtractor[P0], extractorPartial21[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor22[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](fieldExtractor[P0], extractorPartial12[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor13[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](fieldExtractor[P0], extractorPartial03[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor04[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](childrenExtractor[P0], extractorPartial03[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial40[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](fieldExtractor[P0], extractorPartial30[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial31[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](fieldExtractor[P0], extractorPartial21[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial22[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](fieldExtractor[P0], extractorPartial12[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  def extractorPartial13[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](fieldExtractor[P0], extractorPartial03[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial04[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](childrenExtractor[P0], extractorPartial03[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor50[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](fieldExtractor[P0], extractorPartial40[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor41[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](fieldExtractor[P0], extractorPartial31[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor32[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](fieldExtractor[P0], extractorPartial22[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor23[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](fieldExtractor[P0], extractorPartial13[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor14[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](fieldExtractor[P0], extractorPartial04[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor05[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](childrenExtractor[P0], extractorPartial04[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial50[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](fieldExtractor[P0], extractorPartial40[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial41[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](fieldExtractor[P0], extractorPartial31[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial32[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, P4: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](fieldExtractor[P0], extractorPartial22[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial23[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](fieldExtractor[P0], extractorPartial13[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial14[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](fieldExtractor[P0], extractorPartial04[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial05[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](childrenExtractor[P0], extractorPartial04[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam P5 the (Extractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with six members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor60[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](fieldExtractor[P0], extractorPartial50[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with six members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor51[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](fieldExtractor[P0], extractorPartial41[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with six members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor42[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](fieldExtractor[P0], extractorPartial32[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with six members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor33[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](fieldExtractor[P0], extractorPartial23[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with six members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor24[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](fieldExtractor[P0], extractorPartial14[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with six members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor15[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](fieldExtractor[P0], extractorPartial05[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor06[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](childrenExtractor[P0], extractorPartial05[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4, P5) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam P5 the (Extractor) type of the sixth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial60[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: Extractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, B, T](fieldExtractor[P0], extractorPartial50[P1, P2, P3, P4, P5, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members and one auxiliary (non-member) parameter.
   *
   * TESTME
   *
   * @param construct a function (P0, P1, P2, P3, P4, P5) => B => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam P5 the (MultiExtractor) type of the sixth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractorPartial51[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, B, T](fieldExtractor[P0], extractorPartial41[P1, P2, P3, P4, P5, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param elementExtractor the extractor of the first (child) element.
   * @param construct        a function P0 => T, usually the apply method of a case class.
   * @param dropLast         if true, then we drop the last declared field (used when T has an auxiliary member)
   * @param fields           a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam P the (Extractor) type of the first (only) member of the Product type T.
   * @tparam B the type of the auxiliary parameter of T.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial1[P, B, T <: Product : ClassTag](elementExtractor: ElementExtractor[P], construct: P => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] = Extractor {
      (node: Node) =>
          fieldNamesMaybeDropLast(fields, dropLast) match {
              case member :: Nil => for (e0 <- elementExtractor(member).extract(node)) yield construct(e0)
              case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
          }
  } ^^ s"extractorPartial1"

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param elementExtractor        the extractor for the first member to yield its (child) element.
   * @param nestedExtractorFunction a function which is used to create an Extractor for the remaining members.
   * @param construct               a function (P0, P1) => B => T, usually the apply method of a case class.
   * @param dropLast                if true, then we drop the last declared field (used when T has an auxiliary member)
   * @param fields                  a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial2[P0, P1, B, T <: Product : ClassTag](elementExtractor: ElementExtractor[P0], nestedExtractorFunction: (P1 => B => T, List[String]) => Extractor[B => T], construct: (P0, P1) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] = Extractor {
      (node: Node) => {
          fieldNamesMaybeDropLast(fields, dropLast) match {
              case member0 :: fs =>
                  for {
                      e0 <- elementExtractor(member0).extract(node)
                      t <- nestedExtractorFunction(construct.curried(e0), fs).extract(node)
                  } yield t
              case fs => Failure(XmlException(s"extractorPartial2: insufficient field names: $fs")) // TESTME
          }
      }
  } ^^ s"extractorPartial2"

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param elementExtractor        the extractor for the first member to yield its (child) element.
   * @param nestedExtractorFunction a function which is used to create an Extractor for the remaining members.
   * @param construct               a function (P0, P1, P2) => B => T, usually the apply method of a case class.
   * @param dropLast                if true, then we drop the last declared field (used when T has an auxiliary member)
   * @param fields                  a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial3[P0, P1, P2, B, T <: Product : ClassTag](elementExtractor: ElementExtractor[P0], nestedExtractorFunction: ((P1, P2) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] = Extractor {
      (node: Node) => {
          fieldNamesMaybeDropLast(fields, dropLast) match {
              case member0 :: fs =>
                  for {
                      e0 <- elementExtractor(member0).extract(node)
                      t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
                  } yield t
              case fs => Failure(XmlException(s"extractorPartial3: insufficient field names: $fs")) // TESTME
          }
      }
  } ^^ s"extractorPartial3"

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param elementExtractor        the extractor for the first member to yield its (child) element.
   * @param nestedExtractorFunction a function which is used to create an Extractor for the remaining members.
   * @param construct               a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @param dropLast                if true, then we drop the last declared field (used when T has an auxiliary member)
   * @param fields                  a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial4[P0, P1, P2, P3, B, T <: Product : ClassTag](elementExtractor: ElementExtractor[P0], nestedExtractorFunction: ((P1, P2, P3) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- elementExtractor(member0).extract(node)
            t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial4: insufficient field names: $fs")) // TESTME
      }
    }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param elementExtractor        the extractor for the first member to yield its (child) element.
   * @param nestedExtractorFunction a function which is used to create an Extractor for the remaining members.
   * @param construct               a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @param dropLast                if true, then we drop the last declared field (used when T has an auxiliary member)
   * @param fields                  a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial5[P0, P1, P2, P3, P4, B, T <: Product : ClassTag](elementExtractor: ElementExtractor[P0], nestedExtractorFunction: ((P1, P2, P3, P4) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3, P4) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- elementExtractor(member0).extract(node)
            t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial5: insufficient field names: $fs")) // TESTME
      }
    }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * TESTME
   *
   * @param elementExtractor        the extractor for the first member to yield its (child) element.
   * @param nestedExtractorFunction a function which is used to create an Extractor for the remaining members.
   * @param construct               a function (P0, P1, P2, P3, P4, P5) => B => T, usually the apply method of a case class.
   * @param dropLast                if true, then we drop the last declared field (used when T has an auxiliary member)
   * @param fields                  a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam P5 the type of the sixth member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial6[P0, P1, P2, P3, P4, P5, B, T <: Product : ClassTag](elementExtractor: ElementExtractor[P0], nestedExtractorFunction: ((P1, P2, P3, P4, P5) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3, P4, P5) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- elementExtractor(member0).extract(node)
            t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial5: insufficient field names: $fs")) // TESTME
      }
    }
}

/**
 * Companion object to Extractors.
 */
object Extractors extends Extractors {
  /**
   * Preparing the way for when we provide better logging for when things go wrong.
   */
  val flog: Flog = Flog[Extractors]

  class MultiExtractorBase[P: Extractor] extends MultiExtractor[Seq[P]] {
    def extract(nodeSeq: NodeSeq): Try[Seq[P]] = sequence(nodeSeq map Extractor.extract[P])
  }

  /**
   * String multi extractor.
   */
  implicit object stringMultiExtractor extends MultiExtractorBase[String]

  /**
   * Int multi extractor.
   *
   * TESTME
   */
  implicit object IntMultiExtractor extends MultiExtractorBase[Int]

  /**
   * Boolean multi extractor.
   *
   * TESTME
   */
  implicit object BooleanMultiExtractor extends MultiExtractorBase[Boolean]

  /**
   * Double multi extractor.
   *
   * TESTME
   */
  implicit object DoubleMultiExtractor extends MultiExtractorBase[Double]

  /**
   * Long multi extractor.
   *
   * TESTME
   */
  implicit object LongMultiExtractor extends MultiExtractorBase[Long]

  private val extractors: Extractors = new Extractors {}

    implicit lazy val extractOptionalInt: Extractor[Option[Int]] = extractorOption[Int]

    /**
     * Optional string extractor.
     */
    implicit val extractorOptionalString: Extractor[Option[String]] = extractorOption[String]

    /**
   * Method to extract an optional value from a NodeSeq.
   *
   * NOTE: this code looks very wrong. But, as Galileo said, "eppur si muove."
   */
  def extractOptional[P: Extractor](nodeSeq: NodeSeq): Try[P] =
    nodeSeq.headOption map Extractor.extract[P] match {
      case Some(value) => value
      case None => Success(None.asInstanceOf[P])
    }

  /**
   * method to extract a singleton from a NodeSeq.
   */
  def extractSingleton[P: Extractor](nodeSeq: NodeSeq): Try[P] =
    extractSequence[P](nodeSeq) match {
      case Success(p :: Nil) => Success(p)
      // TESTME
      case Success(ps) => Failure(XmlException(s"extractSingleton: non-unique value: $ps"))
      case Failure(x) => Failure(x)
    }

  /**
   * Method which tries to extract a sequence of objects from a NodeSeq.
   *
   * @param nodeSeq a NodeSeq.
   * @tparam P the (Extractor) type to which each individual Node should be converted.
   * @return a Try of Seq[P].
   */
  def extractSequence[P: Extractor](nodeSeq: NodeSeq): Try[Seq[P]] =
    sequence(for (node <- nodeSeq) yield Extractor.extract[P](node))


  /**
   * Return the field names as Seq[String], from either the fields parameter or by reflection into T.
   * Note that fields takes precedence and ClassTag[T] is ignored if fields is used.
   *
   * TODO understand why this no longer seems to be used. It SHOULD be used. Sometimes we should be specifying false.
   *
   * @param fields a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @tparam T the type (typically a case class) from which we will use reflection to get the field names (referred to only if fields is Nil)
   * @return the field names to be used.
   */
  private def fieldNames[T: ClassTag](fields: Seq[String]) = Extractors.fieldNamesMaybeDropLast(fields)

  /**
   * Return the field names as Seq[String], from either the fields parameter or by reflection into T.
   * Note that fields takes precedence and ClassTag[T] is ignored if fields is used.
   *
   * @param fields   a list of field names which, if not empty, is to be used instead of the reflected fields of T (defaults to Nil).
   * @param dropLast true if we should drop the last field from the result of Reflection.extractFieldNames.
   *                 this will occur when we have an auxiliary parameter in our case class.
   * @tparam T the type (typically a case class) from which we will use reflection to get the field names (referred to only if fields is Nil).
   * @return the field names to be used.
   */
  private def fieldNamesMaybeDropLast[T: ClassTag](fields: Seq[String], dropLast: Boolean = false) = fields match {
    case Nil =>
      val result = Reflection.extractFieldNames(implicitly[ClassTag[T]], dropLast).toList
      if (dropLast) result.init else result
    case ps => ps
  }
}
