package com.phasmidsoftware.render

import com.phasmidsoftware.xml.{Extractors, Text}

import scala.annotation.unused
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.matching.Regex

/**
 * Trait which defines generic and standard renderers.
 *
 * NOTE that we should try to merge the various rendererX methods.
 */
trait Renderers {

  def renderer0[R <: Product : ClassTag]: Renderable[R] = (r: R, format: Format, stateR: StateR) => {
    val sb = new mutable.StringBuilder()
    sb.append(format.formatName(open = Some(true), stateR))
    sb.append(format.formatName(open = None, stateR))
    sb.append(r.toString)
    sb.append(format.formatName(open = Some(false), stateR))
    sb.toString()
  }

  def renderer1[P0: Renderable, R <: Product : ClassTag](@unused ignored: P0 => R): Renderable[R] = (r: R, format: Format, stateR: StateR) => {
    val wOuter = renderOuter(r, r.productElement(0).asInstanceOf[P0], 0, format)
    doNestedRender(format, stateR, "", wOuter, r.productElementName(0))
  }

  def renderer2[P0: Renderable, P1: Renderable, R <: Product : ClassTag](construct: (P0, P1) => R): Renderable[R] = (r: R, format: Format, stateR: StateR) => {
    val objectOuter = r.productElement(1).asInstanceOf[P1]
    val constructorInner: P0 => R = construct(_, objectOuter)
    val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0])
    val wInner = renderer1(constructorInner).render(objectInner, format.indent, StateR(true))
    val wOuter = renderOuter(r, objectOuter, 1, format)
    doNestedRender(format, stateR, wInner, wOuter, r.productElementName(1))
  }

  def renderer3[P0: Renderable, P1: Renderable, P2: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2) => R): Renderable[R] = (r: R, format: Format, stateR: StateR) => {
    val objectOuter = r.productElement(2).asInstanceOf[P2]
    val constructorInner: (P0, P1) => R = construct(_, _, objectOuter)
    val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1])
    val wInner = renderer2(constructorInner).render(objectInner, format.indent, StateR(true))
    val wOuter = renderOuter(r, objectOuter, 2, format)
    doNestedRender(format, stateR, wInner, wOuter, r.productElementName(2))
  }

  def renderer4[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3) => R): Renderable[R] = (r: R, format: Format, stateR: StateR) => {
    val objectOuter = r.productElement(3).asInstanceOf[P3]
    val constructorInner: (P0, P1, P2) => R = construct(_, _, _, objectOuter)
    val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2])
    val wInner = renderer3(constructorInner).render(objectInner, format.indent, StateR(true))
    val wOuter = renderOuter(r, objectOuter, 3, format)
    doNestedRender(format, stateR, wInner, wOuter, r.productElementName(3))
  }

  def renderer5[P0: Renderable, P1: Renderable, P2: Renderable, P3: Renderable, P4: Renderable, R <: Product : ClassTag](construct: (P0, P1, P2, P3, P4) => R): Renderable[R] = (r: R, format: Format, stateR: StateR) => {
    val objectOuter = r.productElement(4).asInstanceOf[P4]
    val constructorInner: (P0, P1, P2, P3) => R = construct(_, _, _, _, objectOuter)
    val objectInner = constructorInner(r.productElement(0).asInstanceOf[P0], r.productElement(1).asInstanceOf[P1], r.productElement(2).asInstanceOf[P2], r.productElement(3).asInstanceOf[P3])
    val wInner = renderer4(constructorInner).render(objectInner, format.indent, StateR(true))
    val wOuter = renderOuter(r, objectOuter, 4, format)
    doNestedRender(format, stateR, wInner, wOuter, r.productElementName(4))
  }

  def optionRenderer[R: Renderable]: Renderable[Option[R]] = (ro: Option[R], format: Format, stateR: StateR) => ro match {
    case Some(r) =>
      val wo = stateR.maybeName match {
        case Some(Extractors.optional(x)) => Some(x)
        case Some(x) => Some(x)
        case None => None
      }
      implicitly[Renderable[R]].render(r, format, StateR(wo))
    case None => ""
  }

  private def renderOuter[R <: Product : ClassTag, P: Renderable](r: R, objectOuter: P, indexOuter: Int, format: Format) =
    implicitly[Renderable[P]].render(objectOuter, format.indent, StateR().setName(r, indexOuter))

  private def doNestedRender[R <: Product : ClassTag](format: Format, stateR: StateR, wInner: String, wOuter: String, attributeName: String) = {
    val attribute = attributeName match {
      case Extractors.attribute(_) => true
      case _ => false
    }
    val sb = new mutable.StringBuilder()
    if (!stateR.isInternal) {
      sb.append(format.formatName(open = Some(true), stateR))
      if (!attribute) sb.append(format.formatName(open = None, stateR))
      else sb.append(" ")
    }
    sb.append(wInner)
    if (wInner.nonEmpty) sb.append(format.delimiter)
    sb.append(wOuter)
    if (!stateR.isInternal) {
      if (attribute) sb.append(format.formatName(open = None, stateR))
      sb.append(format.formatName(open = Some(false), stateR))
    }
    sb.toString()
  }

  private def doRenderSequence[R: Renderable](rs: Seq[R], format: Format, maybeName: Option[String]) = {
    val separator = format.sequencer(None)
    val sb = new mutable.StringBuilder()
    sb.append(format.sequencer(Some(true)))
    var first = true
    for (r <- rs) {
      if (!first) sb.append(if (separator == "\n") format.newline else separator)
      sb.append(implicitly[Renderable[R]].render(r, format, StateR(maybeName)))
      first = false
    }
    sb.append(format.newline)
    sb.append(format.sequencer(Some(false)))
    sb.toString()
  }

  /**
   * Method to return a Renderable of Seq[R].
   *
   * @tparam R the underlying element type.
   * @return a Renderable of Seq[R].
   */
  def sequenceRenderer[R: Renderable]: Renderable[Seq[R]] = new Renderable[Seq[R]]() {
    def render(rs: Seq[R], format: Format, stateR: StateR): String = doRenderSequence(rs, format, None)
  }

  /**
   * Method to return a Renderable of Seq[R] with a pre-defined format.
   *
   * NOTE This is required for allowing a format to take precedence over the format parameter passed into the render method.
   *
   * @param formatFunc a function which, given the current state of indents, yields the required format to pass into doRenderSequence.
   * @tparam R the underlying type to be rendered.
   * @return a Renderable of Seq[R].
   */
  def sequenceRendererFormatted[R: Renderable](formatFunc: Int => Format): Renderable[Seq[R]] = (rs: Seq[R], format: Format, stateR: StateR) =>
    doRenderSequence(rs, formatFunc(format.indents), stateR.maybeName)
}

object Renderers {

  val cdata: Regex = """.*([<&>]).*""".r
  implicit val stringRenderer: Renderable[String] = (t: String, _: Format, stateR: StateR) =>
    renderAttribute(
      t match {
        case cdata(_) => s"""<![CDATA[$t]]>"""
        case _ => t
      }, stateR.maybeName)

  implicit val intRenderer: Renderable[Int] = (t: Int, _: Format, stateR: StateR) =>
    renderAttribute(t.toString, stateR.maybeName)

  implicit val booleanRenderer: Renderable[Boolean] = (t: Boolean, _: Format, stateR: StateR) =>
    renderAttribute(t.toString, stateR.maybeName)

  implicit val doubleRenderer: Renderable[Double] = (t: Double, _: Format, stateR: StateR) =>
    renderAttribute(t.toString, stateR.maybeName)

  implicit val longRenderer: Renderable[Long] = (t: Long, _: Format, stateR: StateR) =>
    renderAttribute(t.toString, stateR.maybeName)

  private val renderers = new Renderers {}
  implicit val rendererText: Renderable[Text] = renderers.renderer1(Text)
  implicit val rendererOptionText: Renderable[Option[Text]] = renderers.optionRenderer[Text]

  def maybeAttributeName[R <: Product](r: R, index: Int, useName: Boolean = false): Option[String] =
    r.productElementName(index) match {
      case "$" => None
      case Extractors.attribute(x) => Some(x)
      case x => if (useName) Some(x) else None
    }

  private def renderAttribute(w: String, maybeName: Option[String]): String = maybeName match {
    case Some(name) => s"""$name="$w""""
    case None => w
  }
}

/**
 * Typeclass to specify the required behavior of an object that you want to render as a String.
 *
 * @tparam T the type of the object.
 */
trait Renderable[T] {
  /**
   * This is the method which renders an object t of type T as a String, given three other parameters.
   *
   * @param t         the object to be rendered.
   * @param format    the format to render the object in.
   * @param stateR    the state of rendering.
   * @return a String representation of t.
   */
  def render(t: T, format: Format, stateR: StateR): String
}

trait Format {
  def indent: Format

  val indents: Int

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String

  def delimiter: String = ", "

  def sequencer(open: Option[Boolean]): String

  def newline: String
}

abstract class BaseFormat(indents: Int) extends Format {
  val name: String

  val tab = "  "

  def newline: String = "\n" + (tab * indents)
}

case class FormatXML(indents: Int) extends BaseFormat(indents) {

  val name: String = "FormatXML"

  def indent: Format = copy(indents = indents + 1)

  override def delimiter: String = " "

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = {
    val name = stateR.maybeName.getOrElse(implicitly[ClassTag[T]].runtimeClass.getSimpleName)
    open match {
      case Some(true) => s"<$name"
      case Some(false) => s"</$name>"
      case None => ">"
    }
  }

  def sequencer(open: Option[Boolean]): String = newline
}

case class FormatText(indents: Int) extends BaseFormat(indents) {
  val name: String = "FormatText"

  def indent: Format = copy(indents = indents + 1)

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = open match {
    case Some(true) => "{"
    case Some(false) => "}"
    case None => ""
  }

  def sequencer(open: Option[Boolean]): String = open match {
    case Some(true) => "["
    case Some(false) => "]"
    case None => "\n"
  }
}

// TESTME
case class FormatIndented(indents: Int) extends BaseFormat(indents) {
  val name: String = "FormatIndented"

  def indent: Format = copy(indents = indents + 1)

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = open match {
    case Some(true) => "{"
    case Some(false) => "}"
    case None => ""
  }

  def sequencer(open: Option[Boolean]): String = open match {
    case Some(true) => "["
    case Some(false) => "]"
    case None => ", "
  }
}

case class StateR(maybeName: Option[String], attributes: Map[String, String], interior: Boolean) {
  def dive: StateR = copy(attributes = Map(), interior = true)

  def setName(name: String): StateR = maybeName match {
    case Some(_) => this
    case None => copy(maybeName = Some(name), Map())
  }

  def setName[R <: Product](r: R, index: Int): StateR = copy(maybeName = Renderers.maybeAttributeName(r, index, useName = true), Map())

  def addAttribute(k: String, v: String): StateR = copy(attributes = attributes + (k -> v))

  def isInternal: Boolean = interior
}

object StateR {
  def apply(maybeName: Option[String]): StateR = new StateR(maybeName, Map(), interior = false)

  def apply(interior: Boolean): StateR = new StateR(None, Map(), interior)

  def apply(): StateR = apply(None)
}
