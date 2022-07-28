package com.phasmidsoftware.xml

import com.phasmidsoftware.xml.Extractors.extractField

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

trait Extractors {

  def extractorSequence[P: Extractor](attribute: String): Extractor[Seq[P]] = (node: Node) => Extractors.extractSequence[P](node \ attribute)

  /**
   * Extractor which will convert an Xml Node into an instance of a case object.
   *
   * @param construct a function () => T, usually the apply method of a case object or zero-member case class.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor0[T <: Product : ClassTag](construct: Unit => T): Extractor[T] = (_: Node) => Success(construct())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (P0) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type P0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor1[P0: Extractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] = (node: Node) => fieldNames(fields) match {
    case f :: Nil => extractField[P0](f)(node) map construct
    case fs => Failure(XmlException(s"extractor2: non-unique field name: $fs"))
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
  def extractor2[P0: Extractor, P1: Extractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] = (node: Node) => fieldNames(fields) match {
    case f0 :: fs =>
      for {
        p0 <- extractField[P0](f0)(node)
        t <- extractor1(construct.curried(p0), fs).extract(node)
      } yield t
    case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs"))
  }

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
}

object Extractors {

  implicit object StringExtractor extends Extractor[String] {
    def extract(node: Node): Try[String] = Success(node.text)
  }

  implicit object IntExtractor extends Extractor[Int] {
    def extract(node: Node): Try[Int] = Try(node.text.toInt)
  }

  implicit object BooleanExtractor extends Extractor[Boolean] {
    def extract(node: Node): Try[Boolean] = node.text match {
      case "true" | "yes" | "T" | "Y" => Success(true)
      case _ => Success(false)
    }
  }

  implicit object DoubleExtractor extends Extractor[Double] {
    def extract(node: Node): Try[Double] = Try(node.text.toDouble)
  }

  implicit object LongExtractor extends Extractor[Long] {
    def extract(node: Node): Try[Long] = Try(node.text.toLong)
  }

  def extractSingleton[P: Extractor](nodeSeq: NodeSeq): Try[P] = extractSequence[P](nodeSeq) match {
    case Success(p :: Nil) => Success(p)
    case Success(ps) => Failure(XmlException(s"extractSingleton: non-unique value: $ps"))
    case Failure(x) => Failure(x)
  }

  /**
   * Method to extract a sequence of objects from a NodeSeq.
   *
   * @param nodeSeq a NodeSeq.
   * @tparam P the type to which each Node should be converted .
   * @return a Try of Seq[P].
   */
  def extractSequence[P: Extractor](nodeSeq: NodeSeq): Try[Seq[P]] = {
    val pe: Extractor[P] = implicitly[Extractor[P]]
    val pys: Seq[Try[P]] = for (node <- nodeSeq) yield pe.extract(node)
    val psy: Try[Seq[P]] = Utilities.sequence(pys)
    psy
    //    psy match {
    //      // TODO use transform
    //      case Success(ps) =>
    //        val ps1: Seq[P] = ps
    //        if (ps1.nonEmpty) {
    //          val clazz = ps1.head.getClass
    //          if ( clazz.isAssignableFrom(x))
    //            Success(ps1.asInstanceOf[T])
    //          else Failure(new XmlException(s"incorrect type: $clazz cannot be assigned to $q"))
    //        }
    //        else Success(ps.asInstanceOf[T])
    //      case Failure(x) => Failure(x)
    //    }
  }

  val plural: Regex = """(\w+)s""".r
  val attribute: Regex = """_(\w+)""".r

  def extractField[P: Extractor](field: String)(node: Node): Try[P] = field match {
    // NOTE child nodes are positional. They do not necessarily match names.
    case plural(_) => implicitly[Extractor[P]].extract(node)
    // NOTE attributes must match names where the case class member name starts with "_"
    case attribute(x) => extractSingleton[P](node \ s"@$x")
    // NOTE this is a default case which is currently identical to the plural case.
    case _ => implicitly[Extractor[P]].extract(node)
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

