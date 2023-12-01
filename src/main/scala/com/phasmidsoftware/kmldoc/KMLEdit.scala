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
case class KmlEdit(command: String, operands: Int, op1: Element, maybeOp2: Option[Element])

object KmlEdit {

  def operands(command: String): Int = command match {
    case KmlEdit.JOIN => 2
    case KmlEdit.DELETE => 1
    case _ => 0
  }

  def apply(command: String, op1: Element, maybeOp2: Option[Element]): KmlEdit = KmlEdit(command, operands(command), op1, maybeOp2)

  private val parser = KMLEditParser.apply

  /**
   * Method to parse a line of text as a KmlEdit.
   *
   * @param w the String to be parsed.
   * @return an IO[KmlEdit].
   */
  def parse(w: String): IO[KmlEdit] = IO.fromTry(parser.parseEdit(w -> 0)) // need to get the line sequence number
//    IO(KmlEdit("noop", Element("", ""), None)) // FIXME

  /**
   * Method to parse an iterator of lines of text.
   *
   * @param ws an Iterator[String] typically from Source.getLines.
   * @return a Seq[KmlEdit] wrapped in IO.
   */
  def parseLines(ws: Iterator[String]): IO[Seq[KmlEdit]] = (for (w <- ws) yield parse(w)).toSeq.sequence

  val JOIN = "join"
  val DELETE = "delete"
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
