package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.implicits._
import com.phasmidsoftware.args.Args
import com.phasmidsoftware.core.FP.mapTryGuarded
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLs
import com.phasmidsoftware.kmldoc.KMLEditor.{addExtension, write}
import com.phasmidsoftware.render.FormatXML
import java.io.{BufferedWriter, File, FileWriter, Writer}
import org.slf4j.{Logger, LoggerFactory}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Case class to represent the definition of a KML edit.
 *
 * @param command the function of the edit.
 * @param op1 the target of the edit.
 * @param op2 the result of the edit.
 */
case class KmlEdit(command: String, op1: Element, op2: Option[Element])

object KmlEdit {
  /**
   * Method to parse a line of text as a KmlEdit.
   *
   * @param w the String to be parsed.
   * @return an IO[KmlEdit].
   */
  def parse(w: String): IO[KmlEdit] = IO(KmlEdit("noop", Element("", ""), None)) // FIXME

  /**
   * Method to parse an iterator of lines of text.
   *
   * @param ws an Iterator[String] typically from Source.getLines.
   * @return a Seq[KmlEdit] wrapped in IO.
   */
  def parseLines(ws: Iterator[String]): IO[Seq[KmlEdit]] = (for (w <- ws) yield parse(w)).toSeq.sequence
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
 * @param delimiter     a Regex used to match a delimiter between cells in a row.
 * @param string        a Regex used to match the content of a cell.
 * @param enclosures    the enclosure characters around a list (if any).
 * @param listSeparator the list separator character.
 * @param quote         the quote character which is able to preempt the string regex:
 *                      between two quote characters,
 *                      there can be any number of any character (other than quote).
 * @param verbose       will print the various parameters.
 */
class KMLEditParser(delimiter: Regex, string: Regex, enclosures: String, listSeparator: Char, quote: Char, verbose: Boolean = false) extends JavaTokenParsers {

  if (verbose) KMLEditParser.logger.info(s"delimiter: '${delimiter.regex}', string: '${string.regex}', enclosures: '$enclosures', quote: '$quote', listSeparator: '$listSeparator', ")
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
  def parseEdit(indexedString: (String, Int)): Try[KmlEdit] = parseAll(row, indexedString._1) match {
    case Success(s, _) => scala.util.Success(s)
    case Failure("end of input expected", _) => scala.util.Failure(MultiLineException(indexedString))
    case Failure(x, _) => scala.util.Failure(formException(indexedString, x))
    case Error(x, _) => scala.util.Failure(formException(indexedString, x))
  }

  lazy val row: Parser[KmlEdit] = command ~ element ~ opt(element) ^^ { case c ~ e1 ~ e2o => KmlEdit(c, e1, e2o)}

  lazy val command: Parser[String] = string

  lazy val element: Parser[Element] = tag ~ identifier ^^ { case t ~ i => Element(t, i)}

  lazy val tag: Parser[String] = string

  lazy val identifier: Parser[String] = quotedString

  lazy val cell: Parser[String] = quotedString | list | string | failure("invalid string")

  lazy val quotedString: Parser[String] = quotedStringWithQuotes | pureQuotedString | failure("invalid quoted string")

  private lazy val pureQuotedString: Parser[String] = quote ~> stringInQuotes <~ quote

  private lazy val stringInQuotes: Parser[String] = s"""[^$quote]*""".r

  type Strings = Seq[String]

  lazy val quotedStringWithQuotes: Parser[String] = quotedStringWithQuotesAsList ^^ (ws => ws.mkString(s"$quote"))

  lazy val quotedStringWithQuotesAsList: Parser[Strings] = quote ~> repsep(stringInQuotes, s"$quote$quote") <~ quote

  lazy val list: Parser[String] = getOpenChar ~> (component ~ listSeparator ~ rep1sep(component, listSeparator)) <~ getCloseChar ^^ { case x ~ _ ~ xs => (x +: xs).mkString("{", ",", "}") }

  private val regexComponent = s"""[^,$listSeparator}]+"""
  private lazy val component: Parser[String] = regexComponent.r

  private lazy val getOpenChar: Parser[String] = s"${enclosures.headOption.getOrElse("")}"

  private lazy val getCloseChar: Parser[String] = s"${enclosures.lastOption.getOrElse("")}"

  private def formException(indexedString: (String, Int), x: String) = ParserException(s"Cannot parse row ${indexedString._2}: '${indexedString._1}' due to: $x")

  // XXX used only for debugging
  override def toString: String = s"""LineParser: delimiter=$delimiter, string=$string, listSeparator='$listSeparator', enclosures='$enclosures', quote="$quote""""

  private lazy val getDelimiterChar: Char = {
    @tailrec
    def inner(w: Seq[Char], escaped: Boolean): Char =
      w match {
        case h :: t =>
          if (escaped) h match {
            case 't' => '\t'
            case '\\' => '\\'
            case 'n' => '\n'
            case 'r' => '\r'
            case 'd' => '0'
            case 'f' => '\f'
            case 'b' => '\b'
            case _ => h
          }
          else h match {
            case '[' | '{' => inner(t, escaped = false)
            case '\\' => inner(t, escaped = true)
            case '^' => throw ParserException(s"Cannot get a delimiter from ${delimiter.regex} (unsupported)")
            case _ => h
          }
        case Nil => throw ParserException(s"Cannot get a delimiter from ${delimiter.regex}")
      }

    inner(delimiter.regex.toList, escaped = false)
  }

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
  def apply(implicit c: RowConfig): LineParser = {
    LineParser.logger.info(s"Constructing LineParser with an implicitly defined instance of RowConfig: $c")
    new LineParser(c.delimiter, c.string, c.listEnclosure, c.listSep, c.quote)
  }

  val logger: Logger = LoggerFactory.getLogger(LineParser.getClass)
}

case class ParserException(msg: String, e: Throwable = null) extends Exception(msg, e)

case class MultiLineException[X](x: X) extends Exception(s"multi-line exception: $x")
