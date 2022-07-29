package com.phasmidsoftware.xml

import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.xml.Extractors.{extractField, fieldNames}

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait which defines many useful Extractors.
 */
trait Extractors {

  val flog: Flog = Flog[Extractors]

  import flog._

  def extractorOption[P: Extractor](attribute: String): Extractor[Option[P]] =
    (node: Node) =>
      s"extractorOption: attribute=$attribute, node=$node" !| (implicitly[Extractor[P]].extract(node) match {
        //    s"extractorOption: attribute=$attribute, node=$node" !| (Extractors.extractSequence[P](node \ attribute) match {
        case Success(p) => Success(Some(p))
        //    case Success(Nil) => Success(None)
        //    case Success(ps) => Failure(XmlException(s"extractorOption: too many results: $ps"))
        case Failure(x) => Failure(x)
      })

  /**
   * Extractor which will convert an Xml Node into a sequence of P objects.
   *
   * @tparam P the underlying type of the result.
   * @return an Extractor of Seq[P].
   */
  def extractorSequence[P: Extractor](attribute: String): Extractor[Seq[P]] =
    (node: Node) =>
      s"extractorSequence: attribute=$attribute, node=$node" !| Extractors.extractSequence[P](node \ attribute)

  /**
   * Extractor which will convert an Xml Node into an optional P object.
   *
   * @tparam P the underlying type of the result.
   * @return an Extractor of Option[P].
   */
  def extractorOptional[P: Extractor](attribute: String): Extractor[Option[P]] =
    (node: Node) =>
      s"extractorOptional: attribute=$attribute, node=$node" !| (extractorSequence(attribute).extract(node) map (ps => ps.headOption))

  /**
   * Extractor which will convert an Xml Node (which is ignored) into an instance of a case object.
   *
   * @param construct a function () => T, usually the apply method of a case object or zero-member case class.
   * @tparam T the underlying type of the result, a Product.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor0[T <: Product : ClassTag](construct: Unit => T): Extractor[T] =
    (node: Node) =>
      s"extractor0: node=$node" !| Success(construct())

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with one member.
   *
   * @param construct a function (P0) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first (only) member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with one member of type P0.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor1[P0: Extractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      s"extractor1: node=$node" !| (fieldNames(fields) match {
        case member :: Nil => extractField[P0](member)(node) map construct
        case fs => Failure(XmlException(s"extractor1: non-unique field name: $fs"))
      })

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with two members.
   *
   * @param construct a function (P0,P1) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with two members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor2[P0: Extractor, P1: Extractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      s"extractor2: node=$node" !| (fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor1(construct.curried(p0), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor2: insufficient field names: $fs"))
      })

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
  def extractor3[P0: Extractor, P1: Extractor, P2: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      s"extractor3: node=$node" !| (fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor2(construct(p0, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor3: insufficient field names: $fs"))
      })

  /**
   * Extractor which will convert an Xml Node into an instance of a case class with four members.
   *
   * @param construct a function (P0,P1,P2,P3) => T, usually the apply method of a case class.
   * @tparam P0 the type of the first member of the Product type T.
   * @tparam P1 the type of the second member of the Product type T.
   * @tparam P2 the type of the third member of the Product type T.
   * @tparam P3 the type of the third member of the Product type T.
   * @tparam T  the underlying type of the result, a Product with four members.
   * @return an Extractor[T] whose method extract will convert a Node into a T.
   */
  def extractor4[P0: Extractor, P1: Extractor, P2: Extractor, P3: Extractor, T <: Product : ClassTag](construct: (P0, P1, P2, P3) => T, fields: Seq[String] = Nil): Extractor[T] =
    (node: Node) =>
      s"extractor4: node=$node" !| (fieldNames(fields) match {
        case member0 :: fs =>
          for {
            p0 <- extractField[P0](member0)(node)
            t <- extractor3(construct(p0, _, _, _), fs).extract(node)
          } yield t
        case fs => Failure(XmlException(s"extractor4: insufficient field names: $fs"))
      })
}

/**
 * Companion object to Extractors.
 */
object Extractors {

  private val flog = Flog[Extractors]

  import flog._

  //  private def handler[P,T <: Product: ClassTag, X](member: String): Seq[X] => P = {
  //    val tc = implicitly[ClassTag[T]]
  //    val clazzT = tc.runtimeClass
  //    val f: Field = clazzT.getField(member)
  //    val clazzP: Class[_] = f.getType
  ////    if (clazzT.isAssignableFrom(clazzP))
  //  }

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

  /**
   * method to extract a singleton from a NodeSeq.
   */
  def extractOptional[P: Extractor](nodeSeq: NodeSeq): Try[P] =
    s"extractOptional: $nodeSeq" !| (nodeSeq.headOption map implicitly[Extractor[P]].extract match {
      case Some(value) =>
        value
      case None =>
        Success(None.asInstanceOf[P])
    })
  //    match {
  //      case Some(py) => py
  //      case None => Failure(XmlException("bad exception"))
  //    }
  //      )
  //    extractSequence[P](nodeSeq) match {
  //      case Success(p :: Nil) =>
  //        s"extractOptional1: ${p}" !| Success(Some(p)).asInstanceOf[Try[P]]
  //      case Success(ps) =>
  //        s"extractOptional2: ${ps}" !| Success(None).asInstanceOf[Try[P]]
  //      case Failure(x) => Failure(x)
  //    }

  /**
   * method to extract a singleton from a NodeSeq.
   */
  def extractSingleton[P: Extractor](nodeSeq: NodeSeq): Try[P] =
    extractSequence[P](nodeSeq) match {
      case Success(p :: Nil) => Success(p)
      case Success(ps) => Failure(XmlException(s"extractSingleton: non-unique value: $ps"))
      case Failure(x) => Failure(x)
    }

  /**
   * method to extract a singleton from a NodeSeq.
   */
  def extractAttribute[P: Extractor](nodeSeq: NodeSeq): Try[P] = extractSequence[P](nodeSeq) match {
    case Success(p :: Nil) =>
      Success(p)
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

  /**
   * Method to convert a NodeSeq into a Try[T].
   *
   * @param f       a function which packages a Seq[P] as a T.
   * @param nodeSeq a NodeSeq.
   * @tparam P the type to which each Node should be converted [must be Extractor].
   * @tparam T the underlying result type.
   * @return a Try[T].
   */
  def extractX[P: Extractor, T](f: Seq[P] => T)(nodeSeq: NodeSeq): Try[T] =
    Utilities.sequence(nodeSeq map implicitly[Extractor[P]].extract) map f

  //  /**
  //   * Method to convert a NodeSeq into a Try[T].
  //   *
  //   * @param f a function which packages a Seq[P] as a T.
  //   * @param pys a sequence of Try[P].
  //   * @tparam P the type to which each Node should be converted [must be Extractor].
  //   *                @tparam T the underlying result type.
  //   * @return a Try[T].
  //   */
  //  def handle[P: Extractor, T](f: Seq[P]=>T)(ps: Seq[P]): Try[T] = ps map f

  val plural: Regex = """(\w+)s""".r
  val attribute: Regex = """_(\w+)""".r
  val optional: Regex = """maybe(\w+)""".r

  /**
   * NOTE: ideally, this should be private.
   *
   * @param field the name of a member field.
   * @param node  a Node whence to be extracted.
   * @tparam P the type to which each Node should be converted [must be Extractor].
   * @return a Try[P].
   */
  def extractField[P: Extractor](field: String)(node: Node): Try[P] = field match {
    // NOTE child nodes are positional. They do not necessarily match names.
    case plural(_) => implicitly[Extractor[P]].extract(node)
    // NOTE attributes must match names where the case class member name starts with "_"
    case attribute(x) => extractAttribute[P](node \ s"@$x")
    // NOTE attributes must match names where the case class member name starts with "_"
    case optional(x) =>
      s"extractField(optional): node=$node, x=$x" !| extractOptional[P](node \ x)
    // NOTE this is a default case which is currently identical to the plural case.
    case x =>
      println(node)
      val nodeSeq = node \ x
      extractSingleton[P](nodeSeq)
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

