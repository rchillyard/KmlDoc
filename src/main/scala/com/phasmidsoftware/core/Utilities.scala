package com.phasmidsoftware.core

import cats.effect.IO
import com.phasmidsoftware.kmldoc.Mergeable
import com.phasmidsoftware.xml.{Extractor, Extractors}
import scala.collection.mutable
import scala.util.matching.Regex
import scala.xml.{Elem, Node, NodeSeq}

object Utilities {

  /**
   * Method to filter a sequence of object according to a value and a lens function.
   *
   * CONSIDER is there a method in the standard library which does this?
   *
   * @param lens the function which will extract an A from a T.
   * @param a    the value that must be matched.
   * @param ts   the sequence to be filtered.
   * @tparam T the underlying type of the input and output sequences.
   * @tparam A the type of the value to be matched.
   * @return the filtered version of the sequence.
   */
  def lensFilter[T, A](lens: T => A)(a: A)(ts: Seq[T]): Seq[T] = ts filter (t => lens(t) == a)

  /**
   * The purpose of this method is to allow a String to be parsed as an XML entity, WITHOUT replace " by &quot;
   *
   * @param w the XML string to be parsed.
   * @return an XML element.
   */
  def parseUnparsed(w: String): Elem = {
    val unparsed = scala.xml.Unparsed(w) // NOTE: unparsed really is used (ignore warning).
    <xml>$unparsed</xml>
  }

  private def renderNodeBrief(node: Node): String = node.label

  def renderNode(node: Node, deep: Boolean = false): String = {
    // CONSIDER use SmartBuffer
    val result = new mutable.StringBuilder("node: ")
    result.append(s"label=${node.label}, ")
    result.append(s"length=${node.length}, ")
    result.append(s"descendants=${node.descendant.size}, ")
    result.append(s"attributes=${node.attributes.mkString}, ")
    val children = node.child map (if (deep) renderNode(_, deep) else renderNodeBrief)
    result.append(s"children=${children.mkString("{", ",", "}")}")
    result.toString()
  }

  def renderNodes(nodes: NodeSeq): String = (for (node <- nodes) yield renderNode(node)).mkString("{", ",", "}")

  def show(node: Node)(f: String => Unit = println): IO[Unit] = IO(f(renderNode(node)))
}

case class Text($: CharSequence) extends Mergeable[Text] {
  def merge(t: Text): Option[Text] = Some(Text($.toString ++ t.$.toString))

  override def equals(obj: Any): Boolean = obj match {
    case text: Text => $ == text.$
    case _ => false
  }
}

object Text extends Extractors {

  /**
   * Text extractor.
   */
  implicit val extractorText: Extractor[Text] = extractor10(apply)

  /**
   * Optional text extractor.
   */
  implicit val extractorOptionalText: Extractor[Option[Text]] = extractorOption[Text]

}

class MappableRegex(r: String, f: List[String] => List[String]) extends Regex(r) {
  override def unapplySeq(s: CharSequence): Option[List[String]] =
    super.unapplySeq(s) map f
}

class LowerCaseInitialRegex(r: String) extends MappableRegex(r, ws => ws map { w => w.head.toLower + w.tail })

class XmlBaseException(message: String, cause: Throwable) extends Exception(message, cause)

/**
 * CONSIDER renaming as ExtractorException.
 *
 * @param message the message.
 * @param cause   the cause (or null).
 */
case class XmlException(message: String, cause: Throwable) extends XmlBaseException(message, cause)

object XmlException {
  def apply(message: String): XmlException = apply(message, null)
}

case class MissingFieldException(message: String, reason: String, cause: Throwable) extends XmlBaseException(s"$message ($reason)", cause)