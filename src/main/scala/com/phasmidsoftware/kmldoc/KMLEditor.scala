package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.implicits._
import com.phasmidsoftware.args.Args
import com.phasmidsoftware.core.FP.mapTryGuarded
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLs
import com.phasmidsoftware.kmldoc.KMLEditor.{addExtension, write}
import com.phasmidsoftware.render.FormatXML
import java.io.{BufferedWriter, File, FileWriter, Writer}
import scala.annotation.tailrec
import scala.io.Source
import scala.util._

/**
 * Case class to represent a set of KmlEdit objects.
 *
 * @param edits a sequence of edits.
 */
case class KMLEditor(edits: Seq[KmlEdit]) {

  System.err.println(s"KMLEditor: ${edits.mkString}") // TODO generate log message

  /**
   * Method to process the file defined by baseFilename by parsing it, editing it, and writing it out.
   * The input file extension is ".kml" and the output file suffix is "_out.kml"
   *
   * @param filename the input filename, including the extension (".kml")
   * @return an IO[Unit] which needs to be run.
   */
  def process(filename: Try[String]): IO[Unit] = {
    val kml = """.kml"""
    val baseFilename = filename flatMap (f => if (f.endsWith(kml)) Success(f.replaceAll("""\""" + kml, "")) else Failure(new Exception("Syntax error: filename does not end with .kml")))
    val inputFile = addExtension(baseFilename, kml)
    val outExt = "_out" + kml
    val outputFile = addExtension(baseFilename, outExt)
    inputFile foreach (f => System.err.println(s"KMLEditor.process from $f")) // TODO generate a log message
    outputFile foreach (f => System.err.println(s"KMLEditor.process to $f")) // TODO generate a log message
    processFromTo(inputFile, outputFile)
  }

  /**
   * Method to process the file defined by baseFilename by parsing it, editing it, and writing it out.
   * The input file extension is ".kml" and the output file suffix is "_out.kml"
   *
   * @param inputFile  the input file name, wrapped in Try.
   * @param outputFile the output file name, wrapped in Try.
   * @return an IO[Unit] which needs to be run.
   */
  private def processFromTo(inputFile: Try[String], outputFile: Try[String]): IO[Unit] = {
    val qsi: IO[Seq[Writer]] = for {
      w <- IO.fromTry(outputFile)
      _ = println(w)
      f <- IO(new File(w))
      bW = new BufferedWriter(new FileWriter(f, false))
      ks <- KMLCompanion.loadKML(inputFile)
      ks2 = processKMLs(ks)
      ws <- renderKMLs(ks2, FormatXML(0))
      qs <- write(bW, ws).sequence
    } yield qs

    qsi map (qs => qs.reduce((q, _) => q)) flatMap (q => IO(q.close()))
  }

  /**
   * Method to process a sequence of KML objects.
   * In practice, there is only ever one such object in a KML file.
   *
   * @param ks a sequence of KML.
   * @return a sequence of KML, quite possibly different from the input sequence.
   */
  def processKMLs(ks: Seq[KML]): Seq[KML] = ks map processKML

  /**
   * Method to process a KML object.
   *
   * @param k a KML.
   * @return a sequence of KML, quite possibly different from the input sequence.
   */
  def processKML(k: KML): KML = {
    @tailrec
    def inner(r: KML, es: Seq[KmlEdit]): KML = es match {
      case Nil => r
      case e :: _es => inner(r.edit(e).getOrElse(r), _es)
    }

    inner(k, edits to List)
  }
}

object KMLEditor {
  /**
   * Method to construct a KMLEditor from a filename.
   *
   * @param wy the filename of the edits (wrapped in Try).
   * @return a KMLEditor, wrapped in IO.
   */
  def parse(wy: Try[String]): IO[KMLEditor] = for {
    w <- IO.fromTry(wy)
    s = Source.fromFile(w)
    z <- KmlEdit.parseLines(s.getLines())
    q = KMLEditor(z)
  } yield q

  /**
   * Method to edit a KML file.
   *
   * @param way the command line arguments, wrapped in Try.
   * @return an IO[Unit] which should be run.
   */
  def processKML(way: Try[Args[String]]): IO[Unit] = {
    val wsy = mapTryGuarded[Args[String], Seq[String]](_.size > 1, "size > 1")(_.operands)(way)
    for {
      editor <- parse(wsy map (_.last))
      result <- editor.process(wsy map (_.head))
    } yield result
  }

  private def write(bW: BufferedWriter, ws: Seq[String]): Seq[IO[Writer]] = for (w <- ws) yield IO(bW.append(w))

  private def addExtension(triedBasename: Try[String], ext: String): Try[String] = triedBasename map (_ + ext)
}
