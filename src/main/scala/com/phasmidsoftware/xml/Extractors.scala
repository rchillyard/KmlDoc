package com.phasmidsoftware.xml

import com.phasmidsoftware.xml.Extractors.{extractField, fieldNames}
import com.phasmidsoftware.xml.Utilities.show

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait which defines many useful Extractors.
 */
trait Extractors {

  /**
   * Method to yield an Extractor of Option[P].
   *
   * @param ignored NOTE: unused parameter.
   * @tparam P the underlying type of the result.
   * @return an Extractor of Option[P].
   */
  def extractorOption[P: Extractor](ignored: String): Extractor[Option[P]] =
    (node: Node) =>
      implicitly[Extractor[P]].extract(node) match {
        case Success(p) => Success(Some(p))
        case Failure(x) => Failure(x) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into a sequence of P objects.
   *
   * @param label the label of the child nodes to be returned.
   * @tparam P the underlying type of the result.
   * @return an Extractor of Seq[P].
   */
  def extractorSequence[P: Extractor](label: String): Extractor[Seq[P]] =
    (node: Node) =>
      Extractors.extractSequence[P](node \ label)

  /**
   * Extractor which will convert an Xml Node (which is ignored) into an instance of a case object.
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
   * @param construct a function (P0) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type P0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor10[P0: Extractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member :: Nil =>
          for {
            p0 <- extractField[P0](member)(node)
          } yield construct(p0)
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (P0) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type P0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor01[P0: MultiExtractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member :: Nil =>
          for {
            p0 <- extractMember(node, member)
          } yield construct(p0)
        //          extractMember(node, member) map construct
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs")) // TESTME
      }

  private def extractMember[P: MultiExtractor](node: Node, member: String): Try[P] = implicitly[MultiExtractor[P]].extract(node \ member)

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (P0,P1) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor20[P0: Extractor, P1: Extractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor10(construct.curried(p0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (P0,P1) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor11[P0: Extractor, P1: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor01(construct.curried(p0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (P0,P1) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor02[P0: MultiExtractor, P1: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractMember[P0](node, member0)
            t <- extractor01(construct.curried(p0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor21[P0: Extractor, P1: Extractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor11[P1, P2, T](construct(p0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor12[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor02[P1, P2, T](construct(p0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with three members.
   *
   * @param construct a function (P0,P1,P2) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with three members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor03[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractMember[P0](node, member0)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor02[P1, P2, T](construct(p0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor31[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor21(construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor22[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor12(construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor13[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor03(construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor04[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractMember[P0](node, member0)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor03(construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor32[P0: Extractor, P1: Extractor, P2: Extractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor22(construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor23[P0: Extractor, P1: Extractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor13(construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor14[P0: Extractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor04(construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with five members.
   *
   * @param construct a function (P0,P1,P2,P3,P4) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the fourth member of the Product type T.
   * @tparam P4 the type of the fifth member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with five members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor05[P0: MultiExtractor, P1: MultiExtractor, P2: MultiExtractor, P3: MultiExtractor, P4: MultiExtractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractMember[P0](node, member0)
            // NOTE: do not concern yourself with warnings about implicits here.
            t <- extractor04(construct(p0, _, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor5: insufficient field names: $fs")) // TESTME
      }
}

/**
 * Companion object to Extractors.
 */
object Extractors {
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
   */
  implicit object StringMultiExtractor extends MultiExtractorBase[String]

  /**
   * Int multi extractor.
   */
  implicit object IntMultiExtractor extends MultiExtractorBase[Int]

  /**
   * Boolean multi extractor.
   */
  implicit object BooleanMultiExtractor extends MultiExtractorBase[Boolean]

  /**
   * Double multi extractor.
   */
  implicit object DoubleMultiExtractor extends MultiExtractorBase[Double]

  /**
   * Long multi extractor.
   */
  implicit object LongMultiExtractor extends MultiExtractorBase[Long]

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
   * method to extract an attribute from a NodeSeq.
   */
  def extractAttribute[P: Extractor](nodeSeq: NodeSeq): Try[P] =
    extractSequence[P](nodeSeq) match {
      case Success(p :: Nil) => Success(p)
      // TESTME
      case Success(ps) => Failure(XmlException(s"extractAttribute: non-unique value: $ps"))
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

  /**
   * NOTE: ideally, this should be private but is used for testing and the private method tester is struggling.
   *
   * FIXME: why do attributes, optionals, and defaults use the field but not plurals??
   *
   * @param field the name of a member field.
   * @param node  a Node whence to be extracted.
   * @tparam P the type to which each Node should be converted [must be Extractor].
   * @return a Try[P].
   */
  def extractField[P: Extractor](field: String)(node: Node): Try[P] =
    (field match {
      // NOTE attributes must match names where the case class member name starts with "_"
      case attribute(x) =>
        val nodeSeq = node \ s"@$x"
        "attribute: @$x" -> extractAttribute[P](nodeSeq)
      // NOTE child nodes are positional. They do not necessarily match names.
      case plural(_) =>
        s"plural:" -> implicitly[Extractor[P]].extract(node)
      // NOTE attributes must match names where the case class member name starts with "_"
      case optional(x) =>
        s"optional: $x" -> extractOptional[P](node \ x)
      // NOTE this is a default case which is currently identical to the plural case.
      case x =>
        s"default: $x" -> extractSingleton[P](node \ x)
    }) match {
      case _ -> Success(p) => Success(p)
      case m -> Failure(x) => Failure(XmlException(s"extractField: field=$field, node=${show(node)}, m=$m", x))
    }
}

/**
 * Trait to define the behavior of a type which can be constructed from an XML Node.
 *
 * @tparam T the type to be constructed.
 */
trait Extractor[T] {
  /**
   * Method to convert a Node into a Try[T].
   *
   * @param node a Node.
   * @return a Try[T].
   */
  def extract(node: Node): Try[T]
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