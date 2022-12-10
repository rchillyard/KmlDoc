package com.phasmidsoftware.xml

import com.phasmidsoftware.xml.Extractor.none
import com.phasmidsoftware.xml.Extractors.{MultiExtractorBase, extractChildren, extractField, fieldNames, fieldNamesMaybeDropLast}
import com.phasmidsoftware.xml.Utilities.show
import org.slf4j.{Logger, LoggerFactory}
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.matching.Regex
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
   * @param label the label of the child nodes to be returned.
   * @tparam P the underlying type of the result.
   * @return an Extractor of Seq[P].
   */
  def extractorSequence[P: Extractor](label: String): Extractor[Seq[P]] = (node: Node) => Extractors.extractSequence[P](node \ label)

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
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type E0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor10[E0: Extractor, T <: Product : ClassTag](construct: E0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial1[E0, Unit, T](extractField[E0], e0 => _ => construct(e0), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (M0) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type M0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor01[M0: MultiExtractor, T <: Product : ClassTag](construct: M0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial1[M0, Unit, T](extractChildren[M0], m0 => _ => construct(m0), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first (only) member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial10[E0: Extractor, B, T <: Product : ClassTag](construct: E0 => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial1[E0, B, T](extractField[E0], e0 => b => construct(e0)(b), dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member and one auxiliary (non-member) parameter.
   *
   * @param construct a function (M0) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first (only) member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial01[M0: MultiExtractor, B, T <: Product : ClassTag](construct: M0 => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial1[M0, B, T](extractChildren[M0], m0 => b => construct(m0)(b), dropLast = false, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (E0,E1) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor20[E0: Extractor, E1: Extractor, T <: Product : ClassTag](construct: (E0, E1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial2[E0, E1, Unit, T](extractField[E0], extractorPartial10[E1, Unit, T](_, _), (e0, e1) => _ => construct(e0, e1), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (E0,M0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam M0 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor11[E0: Extractor, M0: MultiExtractor, T <: Product : ClassTag](construct: (E0, M0) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial2[E0, M0, Unit, T](extractField[E0], extractorPartial01[M0, Unit, T](_, _), (e0, e1) => _ => construct(e0, e1), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (M0,M1) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor02[M0: MultiExtractor, M1: MultiExtractor, T <: Product : ClassTag](construct: (M0, M1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial2[M0, M1, Unit, T](extractChildren[M0], extractorPartial01[M1, Unit, T](_, _), (m0, m1) => _ => construct(m0, m1), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial20[E0: Extractor, E1: Extractor, B, T <: Product : ClassTag](construct: (E0, E1) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial2[E0, E1, B, T](extractField[E0], extractorPartial10[E1, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial11[E0: Extractor, M1: MultiExtractor, B, T <: Product : ClassTag](construct: (E0, M1) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial2[E0, M1, B, T](extractField[E0], extractorPartial01[M1, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial02[M0: MultiExtractor, M1: MultiExtractor, B, T <: Product : ClassTag](construct: (M0, M1) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial2[M0, M1, B, T](extractChildren[M0], extractorPartial01[M1, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * CONSIDER re-implement these extractors using extractorPartial30, etc.
   *
   * @param construct a function (E0,E1,E2) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam E2 the (Extractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor30[E0: Extractor, E1: Extractor, E2: Extractor, T <: Product : ClassTag](construct: (E0, E1, E2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[E0, E1, E2, Unit, T](extractField[E0], extractorPartial20[E1, E2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (E0,E1,M0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam M2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor21[E0: Extractor, E1: Extractor, M2: MultiExtractor, T <: Product : ClassTag](construct: (E0, E1, M2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[E0, E1, M2, Unit, T](extractField[E0], extractorPartial11[E1, M2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * TESTME
   *
   * @param construct a function (E0,M0,M1) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam M0 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor12[E0: Extractor, M0: MultiExtractor, M1: MultiExtractor, T <: Product : ClassTag](construct: (E0, M0, M1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[E0, M0, M1, Unit, T](extractField[E0], extractorPartial02[M0, M1, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (M0,M1,M2) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam M2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor03[M0: MultiExtractor, M1: MultiExtractor, M2: MultiExtractor, T <: Product : ClassTag](construct: (M0, M1, M2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial3[M0, M1, M2, Unit, T](extractChildren[M0], extractorPartial02[M1, M2, Unit, T](_, _), (e0, e1, e2) => _ => construct(e0, e1, e2), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0, E1, E2) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam E2 the (Extractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial30[E0: Extractor, E1: Extractor, E2: Extractor, B, T <: Product : ClassTag](construct: (E0, E1, E2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[E0, E1, E2, B, T](extractField[E0], extractorPartial20[E1, E2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0, E1, M2) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam M2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial21[E0: Extractor, E1: Extractor, M2: MultiExtractor, B, T <: Product : ClassTag](construct: (E0, E1, M2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[E0, E1, M2, B, T](extractField[E0], extractorPartial11[E1, M2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (E0, E1, M2) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam M2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial12[E0: Extractor, M1: MultiExtractor, M2: MultiExtractor, B, T <: Product : ClassTag](construct: (E0, M1, M2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[E0, M1, M2, B, T](extractField[E0], extractorPartial02[M1, M2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (M0, M1, M2) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam M1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam M2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial03[M0: MultiExtractor, M1: MultiExtractor, M2: MultiExtractor, B, T <: Product : ClassTag](construct: (M0, M1, M2) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial3[M0, M1, M2, B, T](extractChildren[M0], extractorPartial02[M1, M2, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor40[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractField[P0], extractorPartial30[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor31[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractField[P0], extractorPartial21[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor04[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, Unit, T](extractChildren[P0], extractorPartial03[P1, P2, P3, Unit, T](_, _), (p0, p1, p2, p3) => _ => construct(p0, p1, p2, p3), dropLast = false, fields).extract(node) map (z => z())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial40[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial30[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial31[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial21[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial22[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial12[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractorPartial13[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, B, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => extractorPartial4[P0, P1, P2, P3, B, T](extractField[P0], extractorPartial03[P1, P2, P3, B, T](_, _), construct, dropLast = true, fields).extract(node)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members and one auxiliary (non-member) parameter.
   *
   * @param construct a function (P0, P1, P2, P3) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam B  the type of the non-member parameter of T.
   * @tparam T  the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor50[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor5(extractField[P0], extractor40[P1, P2, P3, P4, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor41[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor5(extractField[P0], extractor31[P1, P2, P3, P4, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor32[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor5(extractField[P0], extractor22[P1, P2, P3, P4, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor23[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor5(extractField[P0], extractor13[P1, P2, P3, P4, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor14[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor5(extractField[P0], extractor04[P1, P2, P3, P4, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor05[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor5(extractChildren[P0], extractor04[P1, P2, P3, P4, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor60[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractField[P0], extractor50[P1, P2, P3, P4, P5, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor51[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractField[P0], extractor41[P1, P2, P3, P4, P5, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor42[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractField[P0], extractor32[P1, P2, P3, P4, P5, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor33[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractField[P0], extractor23[P1, P2, P3, P4, P5, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor24[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractField[P0], extractor14[P1, P2, P3, P4, P5, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor15[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractField[P0], extractor05[P1, P2, P3, P4, P5, T], construct, fields)

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor06[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    nestedExtractor6(extractChildren[P0], extractor05[P1, P2, P3, P4, P5, T], construct, fields)

//  /**
//   * Method to create an Extractor[T] which applies to a case class with 2 parameters and which operates in two parts:
//   * <ol>
//   * <li>extract a value of type P0 from the given node;</li>
//   * <li>create a nested extractor from that e0 value (which extractor extracts one fewer element) and invoke its extract method on the node.</li>
//   * </ol>
//   *
//   * @param extractElementFunction  the function to extract an element: either extractField (for an single element) or extractChildren (for a sequence).
//   * @param nestedExtractorFunction the function to yield a nested extractor (i.e. one which extracts one fewer elements than this).
//   * @param construct               the construct function passed in.
//   * @param fields                  alternative method of defining the fields of T.
//   * @tparam P0 the type of the first element.
//   * @tparam P1 the type of the second element.
//   * @tparam T  the type of the result (must be a Product).
//   * @return an Extractor[T] whose method extract will convert a Node into a T.
//   */
//  private def nestedExtractor2[P0, P1, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: (P1 => T, List[String]) => Extractor[T], construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
//    (node: Node) =>
//      fieldNames(fields) match {
//        case member0 :: fs =>
//          for {
//            e0 <- extractElementFunction(member0)(node)
//            t <- nestedExtractorFunction(construct.curried(e0), fs).extract(node)
//          } yield t
//        case fs => Failure(XmlException(s"nestedExtractor2: insufficient field names: $fs"))
//      }

//  /**
//   * Method to create an Extractor[T] which applies to a case class with 3 parameters and which operates in two parts:
//   * <ol>
//   * <li>extract a value of type P0 from the given node;</li>
//   * <li>create a nested extractor from that e0 value (which extractor extracts one fewer element) and invoke its extract method on the node.</li>
//   * </ol>
//   *
//   * @param extractElementFunction  the function to extract an element: either extractField (for a single element) or extractChildren (for a sequence).
//   * @param nestedExtractorFunction the function to yield a nested extractor (i.e. one which extracts one fewer elements than this).
//   * @param construct               the construct function passed in.
//   * @param fields                  alternative method of defining the fields of T.
//   * @tparam P0 the type of the first element.
//   * @tparam P1 the type of the second element.
//   * @tparam P2 the type of the third element.
//   * @tparam T  the type of the result (must be a Product).
//   * @return an Extractor[T] whose method extract will convert a Node into a T.
//   */
//  private def nestedExtractor3[P0, P1, P2, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2) => T, List[String]) => Extractor[T], construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
//    (node: Node) => {
//      val curried: P0 => P1 => P2 => T = construct.curried
//      fieldNames(fields) match {
//        case member0 :: fs =>
//          for {
//            e0 <- extractElementFunction(member0)(node)
//            eToEToT = FP.uncurry2(curried(e0))
//            t <- nestedExtractorFunction(eToEToT, fs).extract(node)
//          } yield t
//        case fs => Failure(XmlException(s"nestedExtractor3: insufficient field names: $fs"))
//      }
//    }

//  /**
//   * Method to create an Extractor[T] which applies to a case class with 4 parameters and which operates in two parts:
//   * <ol>
//   * <li>extract a value of type P0 from the given node;</li>
//   * <li>create a nested extractor from that e0 value (which extractor extracts one fewer element) and invoke its extract method on the node.</li>
//   * </ol>
//   *
//   * @param extractElementFunction  the function to extract an element: either extractField (for single element) or extractChildren (for a sequence).
//   * @param nestedExtractorFunction the function to yield a nested extractor (i.e. one which extracts one fewer elements than this).
//   * @param construct               the construct function passed in.
//   * @param fields                  alternative method of defining the fields of T.
//   * @tparam P0 the type of the first element.
//   * @tparam P1 the type of the second element.
//   * @tparam P2 the type of the third element.
//   * @tparam P3 the type of the fourth element.
//   * @tparam T  the type of the result (must be a Product).
//   * @return an Extractor[T] whose method extract will convert a Node into a T.
//   */
//  private def nestedExtractor4[P0, P1, P2, P3, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3) => T, List[String]) => Extractor[T], construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
//    (node: Node) => {
//      val curried: P0 => P1 => P2 => P3 => T = construct.curried
//      fieldNames(fields) match {
//        case member0 :: fs =>
//          for {
//            e0 <- extractElementFunction(member0)(node)
//            eToEToT = FP.uncurry3(curried(e0))
//            t <- nestedExtractorFunction(eToEToT, fs).extract(node)
//          } yield t
//        case fs => Failure(XmlException(s"nestedExtractor4: insufficient field names: $fs"))
//      }
//    }

  /**
   * Method to create an Extractor[T] which applies to a case class with 5 parameters and which operates in two parts:
   * <ol>
   * <li>extract a value of type P0 from the given node;</li>
   * <li>create a nested extractor from that e0 value (which extractor extracts one fewer element) and invoke its extract method on the node.</li>
   * </ol>
   *
   * @param extractElementFunction  the function to extract an element: either extractField (for single element) or extractChildren (for a sequence).
   * @param nestedExtractorFunction the function to yield a nested extractor (i.e. one which extracts one fewer elements than this).
   * @param construct               the construct function passed in.
   * @param fields                  alternative method of defining the fields of T.
   * @tparam P0 the type of the first element.
   * @tparam P1 the type of the second element.
   * @tparam P2 the type of the third element.
   * @tparam P3 the type of the fourth element.
   * @tparam P4 the type of the fourth element.
   * @tparam T  the type of the result (must be a Product).
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  private def nestedExtractor5[P0, P1, P2, P3, P4, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3, P4) => T, List[String]) => Extractor[T], construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => {
      val curried: P0 => P1 => P2 => P3 => P4 => T = construct.curried
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            eToEToT = FP.uncurry4(curried(e0))
            t <- nestedExtractorFunction(eToEToT, fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"nestedExtractor5: insufficient field names: $fs"))
      }
    }

  /**
   * Method to create an Extractor[T] which applies to a case class with 6 parameters and which operates in two parts:
   * <ol>
   * <li>extract a value of type P0 from the given node;</li>
   * <li>create a nested extractor from that e0 value (which extractor extracts one fewer element) and invoke its extract method on the node.</li>
   * </ol>
   *
   * @param extractElementFunction  the function to extract an element: either extractField (for single element) or extractChildren (for a sequence).
   * @param nestedExtractorFunction the function to yield a nested extractor (i.e. one which extracts one fewer elements than this).
   * @param construct               the construct function passed in.
   * @param fields                  alternative method of defining the fields of T.
   * @tparam P0 the type of the first element.
   * @tparam P1 the type of the second element.
   * @tparam P2 the type of the third element.
   * @tparam P3 the type of the fourth element.
   * @tparam P4 the type of the fifth element.
   * @tparam P5 the type of the sixth element.
   * @tparam T  the type of the result (must be a Product).
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  private def nestedExtractor6[P0, P1, P2, P3, P4, P5, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3, P4, P5) => T, List[String]) => Extractor[T], construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) => {
      val curried: P0 => P1 => P2 => P3 => P4 => P5 => T = construct.curried
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            eToEToT = FP.uncurry5(curried(e0))
            t <- nestedExtractorFunction(eToEToT, fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"nestedExtractor6: insufficient field names: $fs"))
      }
    }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member plus
   * an "auxiliary" parameter of type B, declared in its own parameter set.
   *
   * @param construct a function P0 => T, usually the apply method of a case class.
   * @tparam P the (Extractor) type of the first (only) member of the Product type T.
   * @tparam B the type of the auxiliary parameter of T.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a B => T.
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
   * @tparam B the type of the auxiliary parameter of T.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[B => T] whose method extract will convert a Node into a B => T.
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
   * @return an Extractor[B => T] whose method extract will convert a Node into a B => T.
   */
  private def extractorPartial3[P0, P1, P2, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            t <- nestedExtractorFunction(FP.uncurry2(construct.curried(e0)), fs).extract(node)
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
   * @return an Extractor[B => T] whose method extract will convert a Node into a B => T.
   */
  private def extractorPartial4[P0, P1, P2, P3, B, T <: Product : ClassTag](extractElementFunction: String => Node => Try[P0], nestedExtractorFunction: ((P1, P2, P3) => B => T, List[String]) => Extractor[B => T], construct: (P0, P1, P2, P3) => B => T, dropLast: Boolean, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) => {
      fieldNamesMaybeDropLast(fields, dropLast) match {
        case member0 :: fs =>
          for {
            e0 <- extractElementFunction(member0)(node)
            t <- nestedExtractorFunction(FP.uncurry3(construct.curried(e0)), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractorPartial4: insufficient field names: $fs")) // TESTME
      }
    }

  // END OF Extractors
}

/**
 * Companion object to Extractors.
 */
object Extractors {

  /**
   * String extractor.
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
    def extract(nodeSeq: NodeSeq): Try[Seq[P]] = Utilities.sequence(nodeSeq map implicitly[Extractor[P]].extract)
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

  implicit val extractorText: Extractor[Text] = new Extractors {}.extractor10(Text)
  implicit val extractorOptionalText: Extractor[Option[Text]] = new Extractors {}.extractorOption[Text]

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
   * Method to extract a sequence of objects from a NodeSeq.
   *
   * @param nodeSeq a NodeSeq.
   * @tparam P the type to which each Node should be converted [must be Extractor].
   * @return a Try of Seq[P].
   */
  def extractSequence[P: Extractor](nodeSeq: NodeSeq): Try[Seq[P]] =
    Utilities.sequence(for (node <- nodeSeq) yield implicitly[Extractor[P]].extract(node))

  val plural: Regex = """(\w+)s""".r
  val attribute: Regex = """_(\w+)""".r
  val optional: Regex = """maybe(\w+)""".r

  private def extractChildren[P: MultiExtractor](member: String)(node: Node): Try[P] = {
    val w = translateMemberName(member)
    val nodeSeq = node \ w
    if (nodeSeq.isEmpty) logger.info(s"extractChildren: no children found for child $w (for member $member) in ${show(node)}")
    implicitly[MultiExtractor[P]].extract(nodeSeq)
  }

  val translations: mutable.HashMap[String, String] = new mutable.HashMap[String, String]()

  def translateMemberName(member: String): String =
    translations.getOrElse(member,
      member match {
        case Extractors.plural(x) => x
        case _ => translations.getOrElse(member, member)
      })

  /**
   * Return the field names as Seq[String], from either the fields parameter or by reflection into T.
   * Note that fields takes precedence and ClassTag[T] is ignored if fields is used.
   *
   * @param fields a list of field names to be used instead of the reflected fields of T.
   * @tparam T the type (typically a case class) from which we will use reflection to get the field names (referred to only if fields is Nil)
   * @return the field names to be used.
   */
  private def fieldNames[T: ClassTag](fields: Seq[String]) = Extractors.fieldNamesMaybeDropLast(fields)

  private def fieldNamesMaybeDropLast[T: ClassTag](fields: Seq[String], dropLast: Boolean = false) = fields match {
    case Nil =>
      val result = Reflection.extractFieldNames(implicitly[ClassTag[T]], dropLast).toList
      if (dropLast) result.init else result
    case ps => ps
  }

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
  def extractField[P: Extractor](field: String)(node: Node): Try[P] =
    (field match {
      // NOTE special name for the (text) content of a node.
      case "$" =>
        "$" -> implicitly[Extractor[P]].extract(node)
      // NOTE attributes must match names where the case class member name starts with "_"
      case attribute("xmlns") => "attribute xmlns" -> Failure(XmlException("it isn't documented by xmlns is a reserved attribute name"))
      case attribute(x) =>
        val pyso = for (ns <- node.attribute(x)) yield for (n <- ns) yield implicitly[Extractor[P]].extract(n)
        s"attribute: $x" -> (pyso match {
          case Some(py :: Nil) => py
          case _ => Failure(XmlException(s"failure to retrieve unique attribute $x from node ${show(node)}"))
        })
      // NOTE child nodes are extracted using extractChildren, not here.
      case plural(x) =>
        s"plural:" -> Failure(XmlException(s"extractField: incorrect usage for plural field: $x. Use extractChildren instead."))
      // NOTE optional members such that the name begins with "maybe"
      case optional(x) =>
        s"optional: $x" -> extractOptional[P](node \ x)
      // NOTE this is the default case which is used for a singleton entity (plural entities would be extracted using extractChildren).
      case x =>
        println(show(node))
        s"singleton: $x" -> extractSingleton[P](node \ x)
    }) match {
      case _ -> Success(p) => Success(p)
      case m -> Failure(x) =>
        val message = s"extractField ($m): field '$field' from node:\n    {${show(node)}}"
        logger.warn(s"$message caused by $x")
        Failure(XmlException(message, x))
    }

  val logger: Logger = LoggerFactory.getLogger(Extractors.getClass)
}

/**
 * Trait to define the behavior of a type which can be constructed from an XML Node.
 *
 * CONSIDER move this to its own top-level.
 *
 * @tparam T the type to be constructed.
 */
trait Extractor[T] {
  self =>

  /**
   * Method to convert a Node into a Try[T].
   *
   * @param node a Node.
   * @return a Try[T].
   */
  def extract(node: Node): Try[T]

  /**
   * Method to combine this Extractor[T] with alt as a disjunctive expression.
   *
   * TESTME
   *
   * @param alt an alternative Extractor which will be invoked if this Extractor fails.
   * @return an Extractor based on this and alt.
   */
  def |(alt: Extractor[T]): Extractor[T] = (node: Node) => self.extract(node) orElse alt.extract(node)

  def orElse[P <: T : Extractor](): Extractor[T] = (node: Node) => self.extract(node) orElse implicitly[Extractor[P]].alt[T].extract(node)

  def alt[P >: T]: Extractor[P] = (node: Node) => self.extract(node)

}

object Extractor {
  def none[T]: Extractor[T] = (_: Node) => Failure(new NoSuchElementException)
}

/**
 * Trait to define the behavior of an iterable type which can be constructed from an XML NodeSeq.
 *
 * @tparam T the (iterable) type to be constructed.
 */
trait MultiExtractor[T] {
  /**
   * Method to convert a NodeSeq into a Try[T], usually an iterable type.
   *
   * @param nodeSeq a NodeSeq.
   * @return a Try[T].
   */
  def extract(nodeSeq: NodeSeq): Try[T]
}

object FP {

  def uncurry2[T1, T2, R](f: T1 => T2 => R): (T1, T2) => R = (t1, t2) => f(t1)(t2)

  def uncurry3[T1, T2, T3, R](f: T1 => T2 => T3 => R): (T1, T2, T3) => R = (t1, t2, t3) => f(t1)(t2)(t3)

  def uncurry4[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): (T1, T2, T3, T4) => R = (t1, t2, t3, t4) => f(t1)(t2)(t3)(t4)

  def uncurry5[T1, T2, T3, T4, T5, R](f: T1 => T2 => T3 => T4 => T5 => R): (T1, T2, T3, T4, T5) => R = (t1, t2, t3, t4, t5) => f(t1)(t2)(t3)(t4)(t5)


}