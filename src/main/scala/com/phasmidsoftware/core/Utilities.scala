package com.phasmidsoftware.core

import scala.collection.mutable
import scala.util.Try
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

    private def renderNodeBrief(node: Node): String = node.label

    def renderNode(node: Node, deep: Boolean = false): String = {
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

    def show(node: Node): Unit = {
        println(renderNode(node))
    }

    /**
     * Method (if needed) to uncurry a 6-level curried function.
     * Not used currently.
     *
     * @param f the original function.
     * @tparam T1 type of parameter 1.
     * @tparam T2 type of parameter 2.
     * @tparam T3 type of parameter 3.
     * @tparam T4 type of parameter 4.
     * @tparam T5 type of parameter 5.
     * @tparam T6 type of parameter 6.
     * @tparam R  the type of the result.
     * @return a function of type (T1, T2, T3, T4, T5, T6) => R
     */
    def uncurry6[T1, T2, T3, T4, T5, T6, R](f: T1 => T2 => T3 => T4 => T5 => T6 => R): (T1, T2, T3, T4, T5, T6) => R = (t1, t2, t3, t4, t5, t6) => f(t1)(t2)(t3)(t4)(t5)(t6)
}

case class Text($: String) {
    override def equals(obj: Any): Boolean = obj match {
        case text: Text => $ == text.$
    }
}

case class XmlException(message: String, cause: Throwable) extends Exception(message, cause)

object XmlException {
  def apply(message: String): XmlException = apply(message, null)
}