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

object KMLDoc extends App {

  import cats.effect.unsafe.implicits.global

  // NOTE: the command line must include two arguments: the base name of the KML file to be processed and the filename for the edits.
  KMLEditor.processKML(Args.parse(args)).unsafeRunSync()
}
