package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.implicits._
import com.phasmidsoftware.args.Args
import com.phasmidsoftware.core.FP.mapTryGuarded
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLs
import com.phasmidsoftware.kmldoc.KMLEditor.{addExtension, write}
import com.phasmidsoftware.render.FormatXML
import java.io.{BufferedWriter, File, FileWriter, Writer}
import scala.io.Source
import scala.util.Try

/**
 * Case class to represent a set of KmlEdit objects.
 *
 * @param edits a sequence of edits.
 */
case class KMLEditor(edits: Seq[KmlEdit]) {

  /**
   * Method to process the file defined by baseFilename by parsing it, editing it, and writing it out.
   * The input file extension is ".kml" and the output file suffix is "_out.kml"
   *
   * @param baseFilename the base filename.
   * @return an IO[Unit] which needs to be run.
   */
  def process(baseFilename: Try[String]): IO[Unit] = {
    val qsi: IO[Seq[Writer]] = for {
      w <- IO.fromTry(addExtension(baseFilename, "_out.kml"))
      f <- IO(new File(w))
      bW = new BufferedWriter(new FileWriter(f, false))
      ks <- KMLCompanion.loadKML(addExtension(baseFilename, ".kml"))
      ks2 = process(ks)
      ws <- renderKMLs(ks2, FormatXML(0))
      qs <- write(bW, ws).sequence
    } yield qs

    qsi map (qs => qs.reduce((q, _) => q)) flatMap (q => IO(q.close()))
  }

  private def process(ks: Seq[KML]): Seq[KML] = ks // FIXME this doesn't edit anything!
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

  private def addExt(basename: String, ext: String): String = basename + ext
}
