package com.phasmidsoftware.render
//import com.phasmidsoftware.flog.Flog

import com.phasmidsoftware.render.Renderers.logger
import com.phasmidsoftware.xml.NamedFunction
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Typeclass to specify the required behavior of an object that you want to render as a String.
 *
 * @tparam T the type of the object.
 */
trait Renderer[T] extends NamedFunction[Renderer[T]] {
  /**
   * This is the method which renders an object t of type T as a String, given three other parameters.
   *
   * @param t      the object to be rendered.
   * @param format the format to render the object in.
   * @param stateR the state of rendering.
   * @return a String representation of t.
   */
  def render(t: T, format: Format, stateR: StateR): Try[String]
}

object Renderer {
//    val flog: Flog = Flog[Renderers]

  /**
   * Method which allows us to wrap a function as a Renderer.
   *
   * @param function the function to be wrapped. It has the same signature as this apply method so it is simply a syntactic convenience.
   * @tparam T the type to be rendered.
   * @return a Renderer[T].
   */
  def apply[T](function: (T, Format, StateR) => Try[String]): Renderer[T] = (t, f, s) => function(t, f, s)

  /**
   * Render the T value using the implicitly found Renderer[T].
   * Debug-log the renderer and its result.
   *
   * @param t      the value to be rendered.
   * @param format the Format.
   * @param stateR the StateR.
   * @tparam T the type of t.
   * @return a String.
   */
  def render[T: Renderer](t: T, format: Format, stateR: StateR): Try[String] =
    implicitly[Renderer[T]].render(t, format, stateR)
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

  private val tab = "  "

  def newline: String = "\n" + (tab * indents)

  protected def getClassName[T: ClassTag](stateR: StateR): String = stateR.maybeName.getOrElse(implicitly[ClassTag[T]].runtimeClass.getSimpleName)
}

case class FormatXML(indents: Int) extends BaseFormat(indents) {

  val name: String = "FormatXML"

  def indent: Format = copy(indents = indents + 1)

  override def delimiter: String = " "

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = {
    val name = getClassName(stateR)
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

  def formatName[T: ClassTag](open: Option[Boolean], stateR: StateR): String = {
    val name = getClassName(stateR)
    open match {
      case Some(true) => s"$name{"
      case Some(false) => "}"
      case None => ""
    }
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

/**
 * Case class intended to take care of the state of rendering.
 * Rendering is complex for several reasons:
 * (1) a method such as render5 invokes render4, render3, etc. in order to process all of the members of a Product.
 * (2) attributes are special and need to be rendered within the opening tag of the top-level element.
 *
 * NOTE: attributes is mutable (it's a StringBuilder). It is retained as we do operations such as setName, recurse.
 * However, we must be careful to ensure that no attribute gets left behind.
 *
 * @param maybeName  an optional String.
 * @param attributes a (private) StringBuilder: accessible via addAttribute or getAttributes.
 * @param interior   false if we are at the top level of an element; false if we have been invoked from above.
 */
case class StateR(maybeName: Option[String], private val attributes: mutable.StringBuilder, interior: Boolean) extends AutoCloseable {

  def setName(name: String): StateR = maybeName match {
    case Some(_) => this
    case None => copy(maybeName = Some(name))
  }

  def recurse: StateR = copy(interior = true)

  def addAttribute(attrString: String): StateR = {
    attributes.append(" " + attrString)
    this
  }

  def getAttributes: String = {
    val result = attributes.toString()
    attributes.clear()
    result
  }

  def setName[R <: Product](r: R, index: Int): StateR = copy(maybeName = Renderers.maybeAttributeName(r, index, useName = true))

  def isInternal: Boolean = interior

  def close(): Unit = {
    if (attributes.toString().trim.nonEmpty) {
      logger.warn(s"StateR.close: attributes not empty: '$attributes'")
    }
  }
}

object StateR {
  def apply(maybeName: Option[String]): StateR = new StateR(maybeName, new mutable.StringBuilder(""), interior = false)

  def apply(interior: Boolean): StateR = new StateR(None, new mutable.StringBuilder(""), interior)

  def apply(): StateR = apply(None)
}
