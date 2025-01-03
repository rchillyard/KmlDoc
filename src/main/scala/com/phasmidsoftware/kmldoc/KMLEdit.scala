package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.implicits._
import org.slf4j.{Logger, LoggerFactory}
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Case class to represent the definition of a KML edit.
 *
 * @param command  the function of the edit.
 * @param operands the number of operands required for this edit.
 * @param op1      the target of the edit.
 * @param maybeOp2 the result of the edit.
 */
case class KmlEdit(command: String, operands: Int, op1: Element, maybeOp2: Option[Element]) {
  def isValid: Boolean = command.nonEmpty && operands > 0
}

object KmlEdit {

  def operands(command: String): Int = command match {
    case JOIN | JOINX => 2
    case DELETE => 1
    case INVERT => 1
    case _ => 0
  }

  /**
   * Process the given feature set and return a new feature set that is the result of the processing.
   *
   * @param edit the Edit to be applied.
   * @param fs   a sequence of Feature, the children of a particular object that extends HasFeatures (i.e. Kml, Document, or Folder).
   * @return a (potentially) different sequence of Feature.
   */
  def editFeatures(edit: KmlEdit, fs: Seq[Feature]): Seq[Feature] = for (f <- fs; z <- f.editToOption(edit, fs)) yield z

  /**
   * Method to create a KmlEdit from a command, an Element, and an optional Element.
   *
   * @param command  the command string.
   * @param op1      the first element.
   * @param maybeOp2 (optional) second element.
   * @return a KmlEdit.
   */
  def apply(command: String, op1: Element, maybeOp2: Option[Element]): KmlEdit = KmlEdit(command, operands(command), op1, maybeOp2)

  private val parser = KMLEditParser.apply

  /**
   * Method to parse a line of text as a KmlEdit.
   *
   * @param w the String to be parsed.
   * @return an IO[KmlEdit].
   */
  def parse(w: String): IO[KmlEdit] = IO.fromTry(parser.parseEdit(w -> 0)) // need to get the line sequence number

  /**
   * Method to parse an iterator of lines of text.
   *
   * @param ws an Iterator[String] typically from Source.getLines.
   * @return a Seq[KmlEdit] wrapped in IO.
   */
  def parseLines(ws: Iterator[String]): IO[Seq[KmlEdit]] = (for (w <- ws) yield parse(w)).toSeq.sequence

  /**
   * Join two elements (typically using the merge method of Mergeable KML objects).
   */
  val JOIN = "join"

  /**
   * like JOIN but retains the first name (i.e. it excludes the second name).
   */
  val JOINX = "joinX"

  /**
   * delete an element.
   */
  val DELETE = "delete"

  /**
   * invert an element.
   */
  val INVERT = "invert"
}

/**
 * Case class to represent a KML element that will take part in an edit.
 *
 * @param tag the tag.
 * @param name the identifier.
 */
case class Element(tag: String, name: String)

/**
 * KMLEditParser: class to parse lines of an edit file.
 * NOTE: list elements always appear as a string in the form { element0 , element1 , ... }
 *
 * NOTE: Much of this is probably overkill (it was copied from TableParser).
 *
 * @param enclosures    the enclosure characters around a list (if any).
 * @param listSeparator the list separator character.
 * @param quote         the quote character which is able to preempt the string regex:
 *                      between two quote characters,
 *                      there can be any number of any character (other than quote).
 * @param verbose       will print the various parameters.
 */
class KMLEditParser(enclosures: String, listSeparator: Char, quote: Char, verbose: Boolean = false) extends JavaTokenParsers {

  if (verbose) KMLEditParser.logger.info(s"enclosures: '$enclosures', quote: '$quote', listSeparator: '$listSeparator', ")
  runChecks()

  override def skipWhitespace: Boolean = false

  /**
   * Method to parse a Row.
   *
   * NOTE: the expression "end of input expected" must be the same as the failure defined in (trait) Parsers: def phrase[T](p: Parser[T]): Parser[T]
   * It's a shame that they didn't make it a constant in Parsers!
   *
   * @param indexedString a tuple of String and Int denoting the line and its index in the file.
   * @return a Try[Strings].
   */
  def parseEdit(indexedString: (String, Int)): Try[KmlEdit] = parseAll(line, indexedString._1) match {
    case Success(s, _) => scala.util.Success(s)
    case Failure("end of input expected", _) => scala.util.Failure(MultiLineException(s"at line ${indexedString._2}: ${indexedString._1}"))
    case Failure(x, _) => scala.util.Failure(formException(indexedString, x))
    case Error(x, _) => scala.util.Failure(formException(indexedString, x))
  }

  lazy val line: Parser[KmlEdit] = command ~ (blank ~> element) ~ opt(blank ~> conjunction ~> blank ~> element) ^^ { case c ~ e1 ~ e2o => KmlEdit(c, e1, e2o) }

  lazy val command: Parser[String] = identifier

  lazy val element: Parser[Element] = (tag <~ blank) ~ identifier ^^ { case t ~ i => Element(t, i) }

  private lazy val conjunction: Parser[String] = "with"

  lazy val tag: Parser[String] = """[^ "]*""".r

  lazy val string: Parser[String] = """[^, "]*""".r

  lazy val blank: Parser[String] = rep(whiteSpace) ^^ (xs => xs.mkString(""))

  private lazy val identifier: Parser[String] = quotedString | string | failure("invalid identifier")

  private lazy val cell: Parser[String] = quotedString | list | string | failure("invalid string")

  lazy val quotedString: Parser[String] = quotedStringWithQuotes | pureQuotedString | failure("invalid quoted string")

  private lazy val pureQuotedString: Parser[String] = quote ~> stringInQuotes <~ quote

  private lazy val stringInQuotes: Parser[String] = s"""[^$quote]*""".r

  private type Strings = Seq[String]

  private lazy val quotedStringWithQuotes: Parser[String] = quotedStringWithQuotesAsList ^^ (ws => ws.mkString(s"$quote"))

  private lazy val quotedStringWithQuotesAsList: Parser[Strings] = quote ~> repsep(stringInQuotes, s"$quote$quote") <~ quote

  private lazy val list: Parser[String] = getOpenChar ~> (component ~ listSeparator ~ rep1sep(component, listSeparator)) <~ getCloseChar ^^ { case x ~ _ ~ xs => (x +: xs).mkString("{", ",", "}") }

  private val regexComponent = s"""[^,$listSeparator}]+"""
  private lazy val component: Parser[String] = regexComponent.r

  private lazy val getOpenChar: Parser[String] = s"${enclosures.headOption.getOrElse("")}"

  private lazy val getCloseChar: Parser[String] = s"${enclosures.lastOption.getOrElse("")}"

  private def formException(indexedString: (String, Int), x: String) = ParserException(s"Cannot parse line ${indexedString._2}: '${indexedString._1}' due to: $x")

  // XXX used only for debugging
  override def toString: String = s"""LineParser: listSeparator='$listSeparator', enclosures='$enclosures', quote="$quote""""

  private lazy val getDelimiterChar: Char = ' '

  private def runChecks(): Unit = {
    def check[X](parser: Parser[X], input: String, matchedValue: X) = Try(parseAll(parser, input) match {
      case Success(`matchedValue`, _) =>
      case Success(z, _) => throw ParserException(s"Warning: (LineParser constructor validity check): '$input' did not result in $matchedValue but instead matched: $z")
      case Failure(z, _) => throw ParserException(s"Warning: (LineParser constructor validity check): '$input' did not result in $matchedValue because: $z")
      case Error(z, _) => throw ParserException(s"Warning: (LineParser constructor validity check): '$input' did not result in $matchedValue because: $z")
    })

    implicit class Trial(x: Try[Unit]) {
      def squawk(): Unit = x match {
        case scala.util.Failure(z) => KMLEditParser.logger.warn(s"squawk: $z")
        case _ =>
      }

      def &&(y: Trial): Trial =
        if (x.isSuccess) y else x
    }

    (
            check(cell, "Hello", "Hello") &&
                    //        check(cell, "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1", "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1") &&
                    check(quotedString, s"""${quote}Hello${getDelimiterChar}Goodbye$quote""", s"""Hello${getDelimiterChar}Goodbye""")
            ).squawk()
  }

}

object KMLEditParser {

  def apply: KMLEditParser = new KMLEditParser("{}", '|', '"')

  val logger: Logger = LoggerFactory.getLogger(KMLEditParser.getClass)
}

case class ParserException(msg: String, e: Throwable = null) extends Exception(msg, e)

case class MultiLineException[X](x: X) extends Exception(s"multi-line exception: $x")


/**
 * Hierarchical trait to satisfy merging.
 *
 * CONSIDER making it a typeclass instead.
 *
 * @tparam T the underlying type.
 */
trait Mergeable[T] {

  /**
   * Merge this mergeable object with <code>t</code>.
   *
   * @param t the object to be merged with this.
   * @return the merged value of T.
   */
  def merge(t: T, mergeName: Boolean = true): Option[T]
}

/**
 * Companion object for Mergeable.
 */
object Mergeable {

  /**
   * Method to merge a Sequence into a Sequence of one.
   *
   * NOTE the merge method assumes that it is associative with respect to T.
   *
   * @param ts   the sequence to be merged.
   * @param fail the value of T used for a merge failure.
   * @tparam T the underlying type (extends Mergeable).
   * @return Seq[T] with only one element.
   */
  def mergeSequence[T <: Mergeable[T]](ts: Seq[T])(fail: => T): Seq[T] =
    Seq(ts reduce[T] ((t1, t2) => (t1 merge t2).getOrElse(fail)))

  /**
   * Method to merge two Optional values of the same type.
   *
   * @param ao the first optional value.
   * @param bo the second optional value.
   * @param f  a method to merge two elements of the underlying type T.
   * @tparam T the underlying type.
   * @return an Option[T].
   */
  def mergeOptions[T](ao: Option[T], bo: Option[T])(f: (T, T) => Option[T]): Option[T] = (ao, bo) match {
    case (Some(a), Some(b)) => f(a, b)
    case (None, _) => bo
    case (_, None) => ao
  }

  /**
   * Method to merge two Optional values of the same type.
   *
   * NOTE: In this method, there is no proper merging of two defined values: we arbitrarily select the first.
   *
   * @param ao the first optional value.
   * @param bo the second optional value.
   * @tparam T the underlying type.
   * @return an Option[T].
   */
  def mergeOptionsBiased[T](ao: Option[T], bo: Option[T]): Option[T] = mergeOptions(ao, bo)((t1, _) => Some(t1))

  /**
   * Method to merge two optional Strings.
   *
   * @param ao the first optional String.
   * @param bo the second optional String.
   * @param f  a function to combine two Strings and yield an Option[String].
   * @return an Option[String].
   */
  def mergeStrings(ao: Option[String], bo: Option[String])(f: (String, String) => Option[String]): Option[String] = mergeOptions(ao, bo)(f)

  /**
   * Method to concatenate two optional Strings with a delimiter between them.
   *
   * @param ao        the first optional String.
   * @param bo        the second optional String.
   * @param delimiter a String to be placed between the two given Strings.
   * @return an Option[String].
   */
  def mergeStringsDelimited(ao: Option[String], bo: Option[String])(delimiter: String): Option[String] = mergeOptions(ao, bo)((a, b) => Some(s"$a$delimiter$b"))
}

/**
 * Trait to satisfy invert command.
 * See also comments re: Mergeable.
 *
 * @tparam T the underlying type.
 */
trait Invertible[T] {
  def invert: Option[T]
}