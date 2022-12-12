package com.phasmidsoftware.core

import scala.collection.mutable
import scala.util.Try
import scala.xml.{Elem, Node}

object Utilities {

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

    /**
     * Method to transform a Seq of Try[X] into a Try of Seq[X].
     *
     * @param xys a Seq of Try[X].
     * @tparam X the underlying type.
     * @return a Try of Seq[X].
     */
    def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
        (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
    }

    def showBrief(node: Node): String = node.label

    def show(node: Node): String = {
        val result = new mutable.StringBuilder("node: ")
        result.append(s"label=${node.label}, ")
        result.append(s"length=${node.length}, ")
        result.append(s"descendants=${node.descendant.size}, ")
        result.append(s"attributes=${node.attributes.mkString}, ")
        val children = node.child map showBrief
        result.append(s"children=${children.mkString("{", ",", "}")}")
        result.toString()
    }
}

case class Text($: String)

case class XmlException(message: String, cause: Throwable) extends Exception(message, cause)

object XmlException {
  def apply(message: String): XmlException = apply(message, null)
}