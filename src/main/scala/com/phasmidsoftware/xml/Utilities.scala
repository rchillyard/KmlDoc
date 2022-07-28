package com.phasmidsoftware.xml

import scala.util.Try
import scala.xml.NodeSeq

object Utilities {
  //  def extract1[X, P: Extractor](n: Node): Seq[X] = ???

  def maybeString(ns: NodeSeq): Option[String] = ns.headOption.map(n => n.text)

  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

}

case class XmlException(message: String, cause: Throwable) extends Exception(message, cause)

object XmlException {
  def apply(message: String): XmlException = apply(message, null)
}