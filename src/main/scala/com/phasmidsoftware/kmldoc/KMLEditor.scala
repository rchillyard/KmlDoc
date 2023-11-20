package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.implicits._
import com.phasmidsoftware.core.FP.optionToTry
import com.phasmidsoftware.kmldoc.KMLCompanion.renderKMLs
import com.phasmidsoftware.render.FormatXML
import java.io.{BufferedWriter, File, FileWriter, Writer}
import scala.util.Try


object KMLEditor extends App {

  import cats.effect.unsafe.implicits.global

  processKML(optionToTry(args.headOption, new Exception("No command-line argument(s)"))).unsafeRunSync()

  private def processKML(basename: Try[String]): IO[Unit] = {
    val qsi: IO[Seq[Writer]] = for {
      w <- IO.fromTry(addExtension(basename, "_out.kml"))
      f <- IO(new File(w))
      bW = new BufferedWriter(new FileWriter(f, false))
      ks <- KMLCompanion.loadKML(addExtension(basename, ".kml"))
      ws <- renderKMLs(ks, FormatXML(0))
      qs <- write(bW, ws).sequence
    } yield qs

    val qi: IO[Writer] = qsi map (qs => qs.reduce((q, _) => q))
    qi flatMap (q => IO(q.close()))
  }

  private def write(bW: BufferedWriter, ws: Seq[String]): Seq[IO[Writer]] = for (w <- ws) yield IO(bW.append(w))

  private def addExtension(triedBasename: Try[String], ext: String): Try[String] = triedBasename map (_ + ext)
}
