package com.phasmidsoftware.render

import com.phasmidsoftware.render.Renderable.newline

import scala.annotation.unused
import scala.collection.mutable
import scala.reflect.ClassTag

trait Renderers {
  def renderer0[R <: Product : ClassTag](format: Format): Renderable[R] = (r: R, _: Int, _: Boolean) => {
    val sb = new mutable.StringBuilder()
    sb.append(format.formatType(true))
    sb.append(r.toString)
    sb.append(format.formatType(false))
    sb.toString()
  }

  def renderer1[P0: Renderable, R <: Product : ClassTag](@unused ignored: P0 => R)(format: Format): Renderable[R] = (r: R, indent: Int, interior: Boolean) => {
    val sb = new mutable.StringBuilder()
    if (!interior) sb.append(format.formatType(true))
    sb.append(implicitly[Renderable[P0]].render(r.productElement(0).asInstanceOf[P0], indent + 1))
    if (!interior) sb.append(format.formatType(false))
    sb.toString()
  }

  def renderer2[P0: Renderable, P1: Renderable, R <: Product : ClassTag](construct: (P0, P1) => R)(format: Format): Renderable[R] = (r: R, indent: Int, interior: Boolean) => {
    val p1 = r.productElement(1).asInstanceOf[P1]
    val renderer1Constructor: P0 => R = construct(_, p1)
    val renderer1Object = renderer1Constructor(r.productElement(0).asInstanceOf[P0])
    val sb = new mutable.StringBuilder()
    if (!interior) sb.append(format.formatType(true))
    sb.append(renderer1(renderer1Constructor)(format).render(renderer1Object, indent + 1, interior = true))
    sb.append(", ")
    sb.append(implicitly[Renderable[P1]].render(p1, indent + 1))
    if (!interior) sb.append(format.formatType(false))
    sb.toString()
  }

  def renderer3[P0: Renderable, P1: Renderable, P2: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2) => R)(format: Format): Renderable[R] = (r: R, indent: Int, interior: Boolean) => {
    val p2 = r.productElement(2).asInstanceOf[P2]
    val renderer2Constructor: (P0, P1) => R = construct(_, _, p2)
    val renderer2Object = renderer2Constructor(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1])
    val sb = new mutable.StringBuilder()
    if (!interior) sb.append(format.formatType(true))
    sb.append(renderer2(renderer2Constructor)(format).render(renderer2Object, indent + 1, interior = true))
    sb.append(", ")
    sb.append(implicitly[Renderable[P2]].render(p2, indent + 1))
    if (!interior) sb.append(format.formatType(false))
    sb.toString()
  }

  def renderer4[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3) => R)(format: Format): Renderable[R] = (r: R, indent: Int, interior: Boolean) => {
    val p3 = r.productElement(3).asInstanceOf[P3]
    val renderer3Constructor: (P0, P1, P2) => R = construct(_, _, _, p3)
    val renderer3Object = renderer3Constructor(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2])
    val sb = new mutable.StringBuilder()
    if (!interior) sb.append(format.formatType(true))
    sb.append(renderer3(renderer3Constructor)(format).render(renderer3Object, indent + 1, interior = true))
    sb.append(", ")
    sb.append(implicitly[Renderable[P3]].render(p3, indent + 1))
    if (!interior) sb.append(format.formatType(false))
    sb.toString()
  }

  def renderer5[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, P4: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => R)(format: Format): Renderable[R] = (r: R, indent: Int, interior: Boolean) => {
    val p4 = r.productElement(4).asInstanceOf[P4]
    val renderer4Constructor: (P0, P1, P2, P3) => R = construct(_, _, _, _, p4)
    val renderer4Object = renderer4Constructor(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2], r.productElement(3).asInstanceOf[P3])
    val sb = new mutable.StringBuilder()
    if (!interior) sb.append(format.formatType(true))
    sb.append(renderer4(renderer4Constructor)(format).render(renderer4Object, indent + 1, interior = true))
    sb.append(", ")
    sb.append(implicitly[Renderable[P4]].render(p4, indent + 1))
    if (!interior) sb.append(format.formatType(false))
    sb.toString()
  }

  implicit val stringRenderer: Renderable[String] = (t: String, _: Int, _: Boolean) => t

  implicit val intRenderer: Renderable[Int] = (t: Int, _: Int, _: Boolean) => t.toString

  implicit val booleanRenderer: Renderable[Boolean] = (t: Boolean, _: Int, _: Boolean) => t.toString

  implicit val doubleRenderer: Renderable[Double] = (t: Double, _: Int, _: Boolean) => t.toString

  implicit val longRenderer: Renderable[Long] = (t: Long, _: Int, _: Boolean) => t.toString

  def optionRenderer[R: Renderable]: Renderable[Option[R]] = (ro: Option[R], indent: Int, _: Boolean) => ro match {
    case Some(r) => implicitly[Renderable[R]].render(r, indent)
    case None => ""
  }

  def sequenceRenderer[R: Renderable](format: Format): Renderable[Seq[R]] = (rs: Seq[R], indent: Int, _: Boolean) => {
    val separator = format.sequencer(None)
    val sb = new mutable.StringBuilder()
    sb.append(format.sequencer(Some(true)))
    val indented = indent + 1
    var first = true
    for (r <- rs) {
      if (!first) sb.append(if (separator == "\n") newline(indent) else separator)
      sb.append(implicitly[Renderable[R]].render(r, indented))
      first = false
    }
    sb.append(newline(indent))
    sb.append(format.sequencer(Some(false)))
    sb.toString()
  }
}

trait Renderable[T] {
  def render(t: T, indent: Int, interior: Boolean = false): String
}

object Renderable {
  def newline(indent: Int): String = " " * indent + "\n"
}

trait Format {
  def formatType[T: ClassTag](open: Boolean): String

  def sequencer(open: Option[Boolean]): String
}

case object FormatXML extends Format {
  def formatType[T: ClassTag](open: Boolean): String = {
    val simpleName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    if (open) s"<$simpleName>"
    else s"</$simpleName>"
  }

  def sequencer(open: Option[Boolean]): String = ""
}

case object FormatFree extends Format {
  def formatType[T: ClassTag](open: Boolean): String = if (open) "{" else "}"

  def sequencer(open: Option[Boolean]): String = open match {
    case Some(true) => "["
    case Some(false) => "]"
    case None => "\n"
  }
}

case object FormatIndented extends Format {
  def formatType[T: ClassTag](open: Boolean): String = if (open) "{" else "}"

  def sequencer(open: Option[Boolean]): String = open match {
    case Some(true) => "["
    case Some(false) => "]"
    case None => ", "
  }
}
