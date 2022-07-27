package com.phasmidsoftware.kmldoc

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}
import scala.xml.{Node, NodeSeq}

object XmlUtils {
//  def extract1[X, P: Extractor](n: Node): Seq[X] = ???

  def maybeString(ns: NodeSeq): Option[String] = ns.headOption.map(n => n.text)

  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = xys.foldLeft(Try(Seq[X]())) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

}
