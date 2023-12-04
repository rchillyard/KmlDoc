package com.phasmidsoftware.core

import com.phasmidsoftware.kmldoc.Mergeable
import com.phasmidsoftware.xml.{Extractor, Extractors}
import scala.collection.mutable
import scala.util.{Success, Try}
import scala.xml.{Node, PCData}

object XML


case class Text($: CharSequence) extends Mergeable[Text] {
  /**
   * Method to merge this and another Text element.
   *
   * @param t the object to be merged with this.
   * @return the merged value of T.
   */
  def merge(t: Text, mergeName: Boolean = true): Option[Text] = ($, t.$) match {
    case (c1: CDATA, c2: CDATA) => c1 merge c2 map (Text(_))
    case _ => Some(Text($.toString + " " + t.$.toString))
  }

  /**
   * Method to determine if this Text matches the given name.
   *
   * @param name the name to match
   * @return true if the this matches name.
   */
  def matches(name: String): Boolean = $ match {
    case c: CDATA => c.content == name
    case x: CharSequence => x.toString == name
    case _ => false
  }

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

object TagProperties {
  def addMustMatch(tag: String): Unit = mustMatchList += tag

  def mustMatch(tag: String): Boolean = mustMatchList.contains(tag)

  private val mustMatchList = mutable.Set[String]()
}

/**
 * Case class to represent a CDATA node.
 *
 * @param content the payload of the CDATA node (will contain <, >, & characters).
 * @param pre     the prefix (probably a newline).
 * @param post    the postfix (probably a newline).
 */
case class CDATA(content: String, pre: String, post: String) extends CharSequence with Mergeable[CDATA] {
  def toXML: Try[String] = Success(s"""$pre<![CDATA[$content]]>$post""")

  def length(): Int = content.length()

  def charAt(index: Int): Char = content.charAt(index)

  def subSequence(start: Int, end: Int): CharSequence = content.subSequence(start, end)

  override def toString: String = s"$pre$content$post"

  /**
   * Merge this mergeable object with <code>t</code>.
   *
   * @param t the object to be merged with this.
   * @return the merged value of T.
   */
  def merge(t: CDATA, mergeName: Boolean = true): Option[CDATA] = Some(CDATA(content + separator(post, t.pre) + t.content, pre, t.post))

  private def separator(a: String, b: String): String = (a + b).replaceAll("\n", "<br>")
}

/**
 * Companion object to CDATA.
 */
object CDATA {

  def apply(x: String, pre: String, post: String): CDATA = new CDATA(x, trimSpace(pre), trimSpace(post))

  def apply(x: String): CDATA = apply(x, "", "")

  def wrapped(x: String): CDATA = apply(x, "\n", "\n")

  private def trimSpace(w: String): String = {
    val sb = new StringBuilder(w)
    SmartBuffer.trimStringBuilder(sb)
    sb.toString()
  }

  def unapply(node: Node): Option[CDATA] = node.child match {
    case Seq(pre, PCData(x), post) => Some(CDATA(x, pre.text, post.text))
    case Seq(PCData(x)) => Some(CDATA(x))
    case _ => None
  }
}