package com.phasmidsoftware.xml

import com.phasmidsoftware.xml.Extractors.{MultiExtractorBase, extractChildren, extractField, fieldNames}
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
   * TESTME
   *
   * @tparam R  result type.
   * @tparam P0 first extractor type.
   * @tparam P1 second extractor type.
   * @return an Extractor[R].
   */
  def extractorAlt[R, P0 <: R : Extractor, P1 <: R : Extractor]: Extractor[R] =
    (node: Node) => implicitly[Extractor[P0]].extract(node) orElse implicitly[Extractor[P1]].extract(node)

  /**
   * Extractor which will convert an Xml Node into a sequence of P objects where there is evidence of Extractor[P].
   *
   * @param label the label of the child nodes to be returned.
   * @tparam P the underlying type of the result.
   * @return an Extractor of Seq[P].
   */
  def extractorSequence[P: Extractor](label: String): Extractor[Seq[P]] =
    (node: Node) =>
      Extractors.extractSequence[P](node \ label)

  /**
   * Method to create a new MultiExtractor based on type P such that the underlying type of the result
   * is Seq[P].
   *
   * @tparam P the underlying (Extractor) type.
   * @return a MultiExtractor of Seq[P]
   */
  def multiExtractor[P: Extractor]: MultiExtractor[Seq[P]] = new MultiExtractorBase[P]()

  /**
   * Extractor which will convert an Xml Node (which is ignored) into an instance of a case object or case class.
   * NOTE that you will have to specify a lambda for the construct function.
   *
   * @param construct a function () => T, usually the apply method of a case object or zero-member case class.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor0[T <: Product : ClassTag](construct: Unit => T): Extractor[T] =
    (_: Node) =>
      Success(construct())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type E0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor10[E0: Extractor, T <: Product : ClassTag](construct: E0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member :: Nil =>
          for {
            e0 <- extractField[E0](member)(node)
          } yield construct(e0)
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (E0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type E0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor10B[B, E0: Extractor, T <: Product : ClassTag](construct: E0 => B => T, fields: Seq[String] = Nil): Extractor[B => T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member :: Nil =>
          for {
            e0 <- extractField[E0](member)(node)
          } yield construct(e0)
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (M0) => T, usually the apply method of a case class.
   * @tparam M0 the (MultiExtractor) type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type M0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor01[M0: MultiExtractor, T <: Product : ClassTag](construct: M0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member :: Nil =>
          for {
            m0 <- extractChildren(node, member)
          } yield construct(m0)
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractField[E0](member0)(node)
            t <- extractor10(construct.curried(e0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs"))
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractField[E0](member0)(node)
            t <- extractor01(construct.curried(e0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            m0 <- extractChildren[M0](node, member0)
            t <- extractor01(construct.curried(m0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (E0,E1,E2) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam E2 the (Extractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor30[E0: Extractor, E1: Extractor, E2: Extractor, T <: Product : ClassTag](construct: (E0, E1, E2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractField[E0](member0)(node)
            t <- extractor20[E1, E2, T](construct(e0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (E0,E1,M0) => T, usually the apply method of a case class.
   * @tparam E0 the (Extractor) type of the first member of the Product type T.
   * @tparam E1 the (Extractor) type of the second member of the Product type T.
   * @tparam M0 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor21[E0: Extractor, E1: Extractor, M0: MultiExtractor, T <: Product : ClassTag](construct: (E0, E1, M0) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractField[E0](member0)(node)
            t <- extractor11[E1, M0, T](construct(e0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            e0 <- extractField[E0](member0)(node)
            t <- extractor02[M0, M1, T](construct(e0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs"))
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the (MultiExtractor) type of the first member of the Product type T.
   * @tparam P1 the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor03[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractChildren[P0](node, member0)
            t <- extractor02[P1, P2, T](construct(p0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor30[P1, P2, P3, T](construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor21[P1, P2, P3, T](construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

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
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor22[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor12[P1, P2, P3, T](construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs"))
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor03[P1, P2, P3, T](construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractChildren[P0](node, member0)
            t <- extractor03[P1, P2, P3, T](construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs"))
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor40[P1, P2, P3, P4, T](construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor22[P1, P2, P3, P4, T](construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs"))
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor13[P1, P2, P3, P4, T](construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor04[P1, P2, P3, P4, T](construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs"))
      }

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
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractChildren[P0](node, member0)
            t <- extractor04[P1, P2, P3, P4, T](construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs"))
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with six members.
   *
   * @param construct a function (P0,P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   * @tparam P0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1 the (Extractor) type of the second member of the Product type T.
   * @tparam P2 the (Extractor) type of the third member of the Product type T.
   * @tparam P3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam P4 the (Extractor) type of the fifth member of the Product type T.
   * @tparam P5 the (Extractor) type of the sixth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor60[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, P4: Extractor, P5: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor50[P1, P2, P3, P4, P5, T](construct(p0, _, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam PE0 the (Extractor) type of the first member of the Product type T.
   * @tparam PE1 the (Extractor) type of the second member of the Product type T.
   * @tparam PE2 the (Extractor) type of the third member of the Product type T.
   * @tparam PE3 the (Extractor) type of the fourth member of the Product type T.
   * @tparam PM0 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam PM1 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T   the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor42[PE0: Extractor, PE1: Extractor, PE2: Extractor, PE3: Extractor, PM0: MultiExtractor, PM1: MultiExtractor, T <: Product : ClassTag](construct: (PE0, PE1, PE2, PE3, PM0, PM1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[PE0](member0)(node)
            t <- extractor32[PE1, PE2, PE3, PM0, PM1, T](construct(p0, _, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs"))
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam PE0 the (Extractor) type of the first member of the Product type T.
   * @tparam PE1 the (Extractor) type of the second member of the Product type T.
   * @tparam PE2 the (MultiExtractor) type of the third member of the Product type T.
   * @tparam PM0 the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam PM1 the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T   the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor33[PE0: Extractor, PE1: Extractor, PE2: Extractor, PM0: MultiExtractor, PM1: MultiExtractor, PM2: MultiExtractor, T <: Product : ClassTag](construct: (PE0, PE1, PE2, PM0, PM1, PM2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[PE0](member0)(node)
            t <- extractor23[PE1, PE2, PM0, PM1, PM2, T](construct(p0, _, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * TESTME
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam PE0 the (Extractor) type of the first member of the Product type T.
   * @tparam P1  the (MultiExtractor) type of the second member of the Product type T.
   * @tparam P2  the (MultiExtractor) type of the third member of the Product type T.
   * @tparam P3  the (MultiExtractor) type of the fourth member of the Product type T.
   * @tparam P4  the (MultiExtractor) type of the fifth member of the Product type T.
   * @tparam T   the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor24[PE0: Extractor, PE1: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (PE0, PE1, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[PE0](member0)(node)
            t <- extractor14[PE1, P1, P2, P3, P4, T](construct(p0, _, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs"))
      }

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
  def extractor06[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, P5: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractChildren[P0](node, member0)
            t <- extractor05[P1, P2, P3, P4, P5, T](construct(p0, _, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs"))
      }

}

/**
 * Companion object to Extractors.
 */
object Extractors {

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

  private def extractChildren[P: MultiExtractor](node: Node, member: String): Try[P] = {
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
  private def fieldNames[T: ClassTag](fields: Seq[String]) = fields match {
    case Nil => Reflection.extractFieldNames(implicitly[ClassTag[T]]).toList
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