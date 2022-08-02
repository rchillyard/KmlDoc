package com.phasmidsoftware.xml

import scala.collection.mutable
import scala.util.Try
import scala.xml.{Node, NodeSeq}

object Utilities {
  //  def extract1[X, P: Extractor](n: Node): Seq[X] = ???

  def maybeString(ns: NodeSeq): Option[String] = ns.headOption.map(n => n.text)

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