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

  private val fileBasename: Try[String] = optionToTry(args.headOption, new Exception("No command-line argument(s)"))

  def addExtension(triedBasename: Try[String], ext: String): Try[String] = triedBasename map (_ + ext)

  val y: IO[String] = IO.fromTry(addExtension(fileBasename, "_out.kml"))
  val z: IO[Seq[KML]] = KMLCompanion.loadKML(addExtension(fileBasename, ".kml"))

  val x: IO[Seq[Writer]] = for {
    f <- y
    p <- IO(new File(f))
    zzz = new BufferedWriter(new FileWriter(p, false))
    fs <- z
    q <- renderKMLs(fs, FormatXML(0))
    a <- write(zzz, q).sequence
  } yield a

  val qq: IO[Writer] = x map (ws => ws.reduce((b, _) => b))

  val zzzz: IO[Unit] = qq flatMap (us => IO(us.close()))
  zzzz.unsafeRunSync()

  def write(file: BufferedWriter, text: Seq[String]): Seq[IO[Writer]] = for {
    w <- text
  } yield IO(file.append(w))


}
