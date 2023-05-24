package com.phasmidsoftware.render

import com.phasmidsoftware.render.Renderer.maybeAttributeName
import com.phasmidsoftware.render.Renderers.logger
import com.phasmidsoftware.xml.{Extractor, NamedFunction}
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
//    val flog: Flog = Flog[Renderer]

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

  /**
   * Method to create a lazy Renderer[T] from an explicit Renderer[T] which is call-by-name.
   * The purpose of this method is to break the infinite recursion caused when implicit values are defined
   * recursively.
   * See the Play JSON library method in JsPath called lazyRead.
   *
   * @param tr a Renderer[T].
   * @tparam T the underlying type of the Renderer required.
   * @return a Renderer[T].
   */
  def createLazy[T](tr: => Renderer[T]): Renderer[T] = (t: T, format: Format, stateR: StateR) => tr.render(t, format, stateR)


  def maybeAttributeName[R <: Product](r: R, index: Int, useName: Boolean = false): Option[String] =
    r.productElementName(index) match {
      case "$" => None
      case Extractor.optionalAttribute(x) => Some(x)
      case Extractor.attribute(x) => Some(x)
      case x => if (useName) Some(x) else None
    }

  def renderAttribute(w: String, maybeName: Option[String]): Try[String] = Try {
    maybeName match {
      case Some(name) => s"""$name="$w""""
      case None => w
    }
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

  /**
   * Method to create a new StateR with a different name.
   *
   * @param name the new name.
   * @return a new StateR.
   */
  def setName(name: String): StateR = maybeName match {
    case Some(_) => this
    case None => copy(maybeName = Some(name))
  }

  /**
   * Method to create a new StateR with interior set to true.
   *
   * @return a new StateR.
   */
  def recurse: StateR = copy(interior = true)

  /**
   * Mutating method to create a this StateR but with attrString appended to the attributes.
   * CONSIDER why is this a mutating method?
   *
   * @param attrString an attribute to add to this StateR.
   * @return a mutated version of this StateR.
   */
  def addAttribute(attrString: String): StateR = {
    attributes.append(" " + attrString)
    this
  }

  /**
   * Mutating method to retrieve the attributes of this StateR and to clear the attributes at the same time.
   *
   * @return the attributes originally stored in this StateR.
   */
  def getAttributes: String = {
    val result = attributes.toString()
    attributes.clear()
    result
  }

  def setName[R <: Product](r: R, index: Int): StateR = copy(maybeName = maybeAttributeName(r, index, useName = true))

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
