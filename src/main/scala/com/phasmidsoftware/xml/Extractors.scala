package com.phasmidsoftware.xml

import com.phasmidsoftware.core.Utilities.{sequence, show}
import com.phasmidsoftware.core.{Reflection, Text, XmlException}
import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.xml.Extractor.none
import com.phasmidsoftware.xml.Extractors.{MultiExtractorBase, extractChildren, extractField, extractSequence, fieldNamesMaybeDropLast}
import org.slf4j.{Logger, LoggerFactory}
import scala.Function.uncurried
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait which defines many useful Extractors, where the result is an instance of Extractor[T].
 */
trait Extractors {

  private val flog = Flog[Extractors]

  import flog._

  /**
   * Method to yield an Extractor of Option[P] where there is evidence of Extractor[P].
   * This is generally used in conjunction with naming a case class member as "maybe"name.
   *
   * @tparam P the underlying type of the result.
   * @return an Extractor of Option[P].
   */
  def extractorOption[P: Extractor]: Extractor[Option[P]] =
    (node: Node) =>
      implicitly[Extractor[P]].extract(node) match {
        case Success(p) => Success(Some(p))
        case Failure(x) => Failure(x) // TESTME
      }

  /**
   * Method to yield an Extractor which can choose from alternate extractors.
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @return an Extractor[R].
   */
  def extractorAlt[R, P0 <: R : Extractor, P1 <: R : Extractor]: Extractor[R] = none[R].orElse[P0]().orElse[P1]()

  /**
   * Extractor which will convert an Xml Node into a sequence of P objects where there is evidence of Extractor[P].
   *
   * NOTE This method is used to extract an iterable from a Node, whereas we use MultiExtractor to extract an iterable from a NodeSeq.
   *
   * @param label the label of the child nodes to be returned.
   * @tparam P the underlying type of the result.
   * @return an Extractor of Iterable[P].
   */
  def extractorIterable[P: Extractor](label: String): Extractor[Iterable[P]] = (node: Node) => extractSequence[P](node \ label)

  /**
   * Method to create a new MultiExtractor based on type P such that the underlying type of the result
   * is Seq[P].
   *
   * @tparam P the underlying (Extractor) type.
   * @return a MultiExtractor of Seq[P]
   */
  def multiExtractor[P: Extractor]: MultiExtractor[Seq[P]] = new MultiExtractorBase[P]()

  /**
   * Method to yield an Extractor[T] where we have an Extractor[B => T].
   * This will occur when we have a case class with an additional parameter set
   * including one parameter of type B.
   *
   * CONSIDER rename this (compose? or extractorCompose?) and couldn't it go inside Extractor?
   *
   * @param extractorBtoT an extractor for the type B => T.
   * @tparam B the type of the value in the additional parameter set of T.
   * @tparam T the underlying type of the resulting extractor.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial[B <: Product : Extractor, T <: Product : ClassTag](extractorBtoT: Extractor[B => T]): Extractor[T] =
    (node: Node) =>
      for {q <- extractorBtoT.extract(node)
           b <- implicitly[Extractor[B]].extract(node)
           } yield q(b)

  /**
   * Extractor which will convert an Xml Node (which is ignored) into an instance of a case object or case class.
   * NOTE that you will have to specify a lambda for the construct function.
   *
   * @param construct a function () => T, usually the apply method of a case object or zero-member case class.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will construct a T while ignoring the input Node.
   */
  def extractor0[T <: Product : ClassTag](construct: Unit => T): Extractor[T] = (_: Node) => Success(construct())

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
  def extractorPartial0[B, T <: Product : ClassTag](construct: B => T): Extractor[B => T] = (_: Node) => Success(construct)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (P0) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor10[P0: Extractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial1[P0, Unit, T](extractField[P0], e0 => _ => construct(e0), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (P0) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a Try[T].
   */
  def extractor01[P0: MultiExtractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial1[P0, Unit, T](extractChildren[P0], m0 => _ => construct(m0), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial1[P0, B, T](extractField[P0], e0 => b => construct(e0)(b), dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial1[P0, B, T](extractChildren[P0], m0 => b => construct(m0)(b), dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial2[P0, P1, Unit, T](extractField[P0], extractorPartial10[P1, Unit, T](_, _), (e0, e1) => _ => construct(e0, e1), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial2[P0, P1, Unit, T](extractField[P0], extractorPartial01[P1, Unit, T](_, _), (e0, e1) => _ => construct(e0, e1), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial2[P0, P1, Unit, T](extractChildren[P0], extractorPartial01[P1, Unit, T](_, _), (m0, m1) => _ => construct(m0, m1), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial2[P0, P1, B, T](extractField[P0], extractorPartial10[P1, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial2[P0, P1, B, T](extractField[P0], extractorPartial01[P1, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial2[P0, P1, B, T](extractChildren[P0], extractorPartial01[P1, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](extractField[P0], extractorPartial20[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())
//    (node: Node) => extractorPartial30[P0,P1,P2,Unit,T]((e0, e1, e2) => _ => construct(e0, e1, e2), fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](extractField[P0], extractorPartial11[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](extractField[P0], extractorPartial02[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial3[P0, P1, P2, Unit, T](extractChildren[P0], extractorPartial02[P1, P2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](extractField[P0], extractorPartial20[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](extractField[P0], extractorPartial11[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](extractField[P0], extractorPartial02[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial3[P0, P1, P2, B, T](extractChildren[P0], extractorPartial02[P1, P2, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractField[P0], extractorPartial30[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractField[P0], extractorPartial21[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractField[P0], extractorPartial12[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractField[P0], extractorPartial03[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractChildren[P0], extractorPartial03[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial30[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial21[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial12[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial03[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractChildren[P0], extractorPartial03[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](extractField[P0], extractorPartial40[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](extractField[P0], extractorPartial31[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](extractField[P0], extractorPartial22[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](extractField[P0], extractorPartial13[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](extractField[P0], extractorPartial04[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, Unit, T](extractChildren[P0], extractorPartial04[P1, P2, P3, P4, Unit, T](_, _), (p0, p1, p2, p3, p4) => _ => construct(p0, p1, p2, p3, p4), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](extractField[P0], extractorPartial40[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](extractField[P0], extractorPartial31[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](extractField[P0], extractorPartial22[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](extractField[P0], extractorPartial13[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](extractField[P0], extractorPartial04[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial5[P0, P1, P2, P3, P4, B, T](extractChildren[P0], extractorPartial04[P1, P2, P3, P4, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractField[P0], extractorPartial50[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractField[P0], extractorPartial41[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractField[P0], extractorPartial32[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractField[P0], extractorPartial23[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractField[P0], extractorPartial14[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractField[P0], extractorPartial05[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, Unit, T](extractChildren[P0], extractorPartial05[P1, P2, P3, P4, P5, Unit, T](_, _), (p0, p1, p2, p3, p4, p5) => _ => construct(p0, p1, p2, p3, p4, p5), dropLast = false, fields).extract(node) map (z => z())

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, B, T](extractField[P0], extractorPartial50[P1, P2, P3, P4, P5, B, T](_, _), construct, dropLast = true, fields).extract(node)

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
    (node: Node) => extractorPartial6[P0, P1, P2, P3, P4, P5, B, T](extractField[P0], extractorPartial41[P1, P2, P3, P4, P5, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param construct a function P0 => T, usually the apply method of a case class.
   * @tparam P the (Extractor) type of the first (only) member of the Product type T.
   * @tparam B the type of the auxiliary parameter of T.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial1[P, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P], construct: P => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) =>
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member :: Nil =>
          for {
            e0 <- extractElementFunction(member)(node)
          } yield construct(e0)
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param construct a function (P0, P1) => B => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial2[P0, P1, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: (P1 => B => T, List[String]) => Extractor[B => T], construct: (P0, P1) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            t <- nestedExtractorFunction(construct.curried(e0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial2: insufficient field names: $fs")) // TESTME
      }
    }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param construct a function (P0, P1, P2) => B => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial3[P0, P1, P2, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial3: insufficient field names: $fs")) // TESTME
      }
    }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param construct a function (P0, P1, P2, P3) => B => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial4[P0, P1, P2, P3, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial4: insufficient field names: $fs")) // TESTME
      }
    }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param construct a function (P0, P1, P2, P3, P4) => B => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam B  the type of the auxiliary parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a Try[B => T].
   */
  private def extractorPartial5[P0, P1, P2, P3, P4, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3, P4) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3, P4) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
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
   * @param construct a function (P0, P1, P2, P3, P4, P5) => B => T, usually the apply method of a case class.
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
  private def extractorPartial6[P0, P1, P2, P3, P4, P5, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3, P4, P5) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3, P4, P5) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            t <- nestedExtractorFunction(uncurried(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial5: insufficient field names: $fs")) // TESTME
      }
    }
}

/**
 * Companion object to Extractors.
 */
object Extractors {

  /**
   * String extractor.
   * TESTME
   */
  implicit object UnitExtractor extends Extractor[Unit] {
    def extract(node: Node): Try[Unit] = Success(())
  }

  /**
   * String extractor.
   */
  implicit object StringExtractor extends Extractor[String] {
    def extract(node: Node): Try[String] = Success(node.text)
  }

  /**
   * Int extractor.
   */
  implicit object IntExtractor extends Extractor[Int] {
    def extract(node: Node): Try[Int] = Try(node.text.toInt)
  }

  /**
   * Boolean extractor.
   */
  implicit object BooleanExtractor extends Extractor[Boolean] {
    def extract(node: Node): Try[Boolean] = node.text match {
      case "true" | "yes" | "T" | "Y" => Success(true)
      case _ => Success(false)
    }
  }

  /**
   * Double extractor.
   */
  implicit object DoubleExtractor extends Extractor[Double] {
    def extract(node: Node): Try[Double] = Try(node.text.toDouble)
  }

  /**
   * Long extractor.
   */
  implicit object LongExtractor extends Extractor[Long] {
    def extract(node: Node): Try[Long] = Try(node.text.toLong)
  }

  class MultiExtractorBase[P: Extractor] extends MultiExtractor[Seq[P]] {
    def extract(nodeSeq: NodeSeq): Try[Seq[P]] = sequence(nodeSeq map implicitly[Extractor[P]].extract)
  }

  /**
   * String multi extractor.
   *
   * TESTME
   */
  implicit object StringMultiExtractor extends MultiExtractorBase[String]

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

  /**
   * Text extractor.
   */
  implicit val extractorText: Extractor[Text] = new Extractors {}.extractor10(Text)

  /**
   * Optional text extractor.
   */
  implicit val extractorOptionalText: Extractor[Option[Text]] = new Extractors {}.extractorOption[Text]

  /**
   * Optional string extractor.
   */
  implicit val extractorOptionalString: Extractor[Option[String]] = new Extractors {}.extractorOption[String]

  /**
   * Method to extract an optional value from a NodeSeq.
   *
   * NOTE: this code looks very wrong. But, as Galileo said, "epur se muove."
   */
  def extractOptional[P: Extractor](nodeSeq: NodeSeq): Try[P] =
    nodeSeq.headOption map implicitly[Extractor[P]].extract match {
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
    sequence(for (node <- nodeSeq) yield implicitly[Extractor[P]].extract(node))

  /**
   * Method to yield a Try[P] for a particular child or attribute of the given node.
   *
   * NOTE: Plural members should use extractChildren and not extractField.
   *
   * NOTE: ideally, this should be private but is used for testing and the private method tester is struggling.
   *
   * @param field the name of a member field:
   *              if the (text) content of the node, then the field should be "$";
   *              if a singleton child, then field is as is;
   *              if an attribute, then field should begin with "_";
   *              if an optional child, then field should begin with "maybe".
   * @param node  a Node whence field is to be extracted.
   * @tparam P the type to which Node should be converted [must be Extractor].
   * @return a Try[P].
   */
  def extractField[P: Extractor](field: String)(node: Node): Try[P] = doExtractField[P](field, node) match {
    case _ -> Success(p) => Success(p)
    case m -> Failure(x) =>
      x match {
        case _: NoSuchFieldException => Success(None.asInstanceOf[P])
        case _ =>
          val message = s"extractField ($m): field '$field' from node:\n    {${show(node)}}"
          logger.warn(s"$message caused by $x")
          Failure(XmlException(message, x))
      }
  }
  /**
   * Regular expression to match a plural name, viz. .....s
   */
  val plural: Regex = """(\w+)s""".r

  /**
   * Regular expression to match an attribute name, viz. _.....
   */
  val attribute: Regex = """_(\w+)""".r

  /**
   * Regular expression to match an optional attribute name, viz. _.....
   * With an optional attribute, it will have a default value that does not need to be overridden.
   */
  val optionalAttribute: Regex = """__(\w+)""".r

  /**
   * Regular expression to match an optional name, viz. maybe....
   */
  val optional: Regex = """maybe(\w+)""".r

  val logger: Logger = LoggerFactory.getLogger(Extractors.getClass)

  val translations: mutable.HashMap[String, String] = new mutable.HashMap[String, String]()

  def translateMemberName(member: String): String =
    translations.getOrElse(member,
      member match {
        case Extractors.plural(x) => x
        case _ => translations.getOrElse(member, member)
      })

  private def extractChildren[P: MultiExtractor](member: String)(node: Node): Try[P] = {
    val w = translateMemberName(member)
    val nodeSeq = node \ w
    if (nodeSeq.isEmpty) logger.info(s"extractChildren: no children found for child $w (for member $member) in ${show(node)}")
    implicitly[MultiExtractor[P]].extract(nodeSeq)
  }

  private def doExtractField[P: Extractor](field: String, node: Node) = field match {
    // NOTE special name for the (text) content of a node.
    case "$" => "$" -> extractText[P](node)
    // NOTE attributes must match names where the case class member name starts with "_"
    case attribute("xmlns") => "attribute xmlns" -> Failure(XmlException("it isn't documented by xmlns is a reserved attribute name"))
    case optionalAttribute(x) => s"optional attribute: $x" -> extractAttribute[P](node, x, optional = true)
    case attribute(x) => s"attribute: $x" -> extractAttribute[P](node, x)
    // NOTE child nodes are extracted using extractChildren, not here.
    case plural(x) => s"plural:" -> Failure(XmlException(s"extractField: incorrect usage for plural field: $x. Use extractChildren instead."))
    // NOTE optional members such that the name begins with "maybe"
    case optional(x) => s"optional: $x" -> extractOptional[P](node \ x)
    // NOTE this is the default case which is used for a singleton entity (plural entities would be extracted using extractChildren).
    case x => s"singleton: $x" -> extractSingleton[P](node \ x)
  }

  private def extractText[P: Extractor](node: Node): Try[P] = implicitly[Extractor[P]].extract(node)

  private def extractAttribute[P: Extractor](node: Node, x: String, optional: Boolean = false): Try[P] =
    (for (ns <- node.attribute(x)) yield for (n <- ns) yield implicitly[Extractor[P]].extract(n)) match {
      case Some(py :: Nil) => py
      case _ if optional => Failure(new NoSuchFieldException)
      case _ => Failure(XmlException(s"failure to retrieve unique attribute $x from node ${show(node)}"))
    }

  /**
   * Return the field names as Seq[String], from either the fields parameter or by reflection into T.
   * Note that fields takes precedence and ClassTag[T] is ignored if fields is used.
   *
   * TODO understand why this no longer seems to be used. It SHOULD be used. Sometimes we should be specifying false.
   *
   * @param fields a list of field names to be used instead of the reflected fields of T.
   * @tparam T the type (typically a case class) from which we will use reflection to get the field names (referred to only if fields is Nil)
   * @return the field names to be used.
   */
  private def fieldNames[T: ClassTag](fields: Seq[String]) = Extractors.fieldNamesMaybeDropLast(fields)

  /**
   * Return the field names as Seq[String], from either the fields parameter or by reflection into T.
   * Note that fields takes precedence and ClassTag[T] is ignored if fields is used.
   *
   * @param fields   a list of field names to be used instead of the reflected fields of T.
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
