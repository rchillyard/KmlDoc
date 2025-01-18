package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.args.Args

/**
 * Runnable object for processing KML files with specified edits.
 *
 * This object acts as an entry point for the application and takes two command-line arguments:
 * the base name of the KML file to be processed and the filename containing the edits to apply.
 *
 * If the required command-line arguments are not provided, an error message is displayed.
 * When the correct inputs are provided, the KML file is processed by delegating to the `KMLEditor` object.
 */
object KMLDoc extends App {

  if (args.length < 2) System.err.println(s"Syntax: KMLDoc basename edits")

  else {

    import cats.effect.unsafe.implicits.global

    // NOTE: the command line must include two arguments: the base name of the KML file to be processed and the filename for the edits.
    val arguments = Args.parse(args)
    KMLEditor.processKML(arguments).unsafeRunSync()
  }
}
