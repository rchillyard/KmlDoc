package com.phasmidsoftware.kmldoc

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

trait Extractors {
  val plural: Regex = """(\w+)s""".r


  def extractorSeq[P: Extractor](attribute: String): Extractor[Seq[P]] = (node: Node) => Extractors.extractSeq[P](node \ attribute)

  def extractor1[P0: Extractor, T <: Product : ClassTag](construct: P0 => T, fields: Seq[String] = Nil): Extractor[T] = new Extractor[T] {
    val p0e: Extractor[P0] = implicitly[Extractor[P0]]

    def extractMany(node: Node): Try[Seq[T]] =
      fieldNames(fields) match {
        case plural(f0, _) :: Nil =>
          //          val value: Try[Seq[P0]] = Extractors.extractSeq[P0](node \ f0)
          val value = p0e.extractOne(node)
          value map construct
        //          val pys: Seq[Try[P0]] = for (node <- seq) yield pe.extractOne(node)
        //          val psy: Try[Seq[P0]] = XmlUtils.sequence(pys)
        //          val p: Try[Seq[T]] = psy match {
        //            // TODO use transform
        //            case Success(ps) =>
        //              val ps1: Seq[P0] = ps
        //              val q: Seq[T] = ps1 map construct
        //              Success(q)
        //            case Failure(x) => Failure(x)
        //          }
        //          p
        case f0 :: Nil =>
          val value = p0e.extractOne(node)
          value map construct

        case fs => Failure(new Exception(s"non-unique field name: $fs"))
      }

  }

//  /**
//   * Method to return a CellParser[T] where T is a 2-ary Product and which is based on a function to convert a (P0,P1) into a T.
//   *
//   * @param construct a function (P0,P1) => T, usually the apply method of a case class.
//   * @tparam P0 the type of the first field of the Product type T.
//   * @tparam P1 the type of the second field of the Product type T.
//   * @tparam T  the underlying type of the result, a Product.
//   * @return a MultiCellParser which converts Strings from a Row into the field types P0 and P1 and thence into a T
//   */
  //  def extractor2[P0: Extractor, P1: Extractor, T <: Product : ClassTag](construct: (P0, P1) => T, fields: Seq[String] = Nil): Extractor[T] = new Extractor[T] {
  //    val pe: Extractor[P0] = implicitly[Extractor[P0]]
  //    def extractOne(node: Node): Try[T] =
  //        fieldNames(fields) match {
  //          case f0 :: fs =>
  //            for {
  //              p0 <- pe.extractOne(node)(f0)
  //              t <- extractor1(construct.curried(p0), fs).extractOne(node)
  //            } yield t
  //          case _ => Failure(Exception("no field names"))
  //        }
  //    }


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

  implicit object IntExtractor extends Extractor[Int] {
//    def extractText[P: (node: Node): Try[Int] = Try(implicitly[Numeric[Int]].parseString(node.text).get)
  }

  /**
   * Method to extractOne a sequence of objects from a NodeSeq.
   *
   * @param nodeSeq a NodeSeq.
   * @tparam P the type to which each Node should be converted .
   * @return a Try of Seq[P].
   */
  def extractSeq[P: Extractor](nodeSeq: NodeSeq): Try[Seq[P]] = {
    val pe: Extractor[P] = implicitly[Extractor[P]]
    val pys: Seq[Try[P]] = for (node <- nodeSeq) yield pe.extractOne(node)
    val psy: Try[Seq[P]] = XmlUtils.sequence(pys)
    psy
    //    psy match {
    //      // TODO use transform
    //      case Success(ps) =>
    //        val ps1: Seq[P] = ps
    //        if (ps1.nonEmpty) {
    //          val clazz = ps1.head.getClass
    //          if ( clazz.isAssignableFrom(x))
    //            Success(ps1.asInstanceOf[T])
    //          else Failure(new Exception(s"incorrect type: $clazz cannot be assigned to $q"))
    //        }
    //        else Success(ps.asInstanceOf[T])
    //      case Failure(x) => Failure(x)
    //    }
  }

}

trait Extractor[T] {
  def extractOne(node: Node): Try[T] = extractMany(node) match {
    case Success(t :: Nil) => Success(t)
    case Success(x) => Failure(new Exception(s"extractOne: not unique: $x"))
    case Failure(x) => Failure(x)
  }

  def extractMany(node: Node): Try[Seq[T]]

  def extractText[P : { def parseString(x: String): Option[P] }]  (node: Node): Try[P] = Try(parseString(node.text).get)

}

trait MultiExtractor[T] extends Extractor[T] {

}
