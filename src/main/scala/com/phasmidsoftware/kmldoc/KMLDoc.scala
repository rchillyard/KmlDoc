package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.args.Args

object KMLDoc extends App {

  if (args.length < 2) System.err.println(s"Syntax: KMLDoc basename edits")

  else {

    import cats.effect.unsafe.implicits.global

    // NOTE: the command line must include two arguments: the base name of the KML file to be processed and the filename for the edits.
    val arguments = Args.parse(args)
    KMLEditor.processKML(arguments).unsafeRunSync()
  }
}
