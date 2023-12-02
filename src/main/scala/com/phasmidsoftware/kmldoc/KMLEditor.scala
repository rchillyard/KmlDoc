package com.phasmidsoftware.kmldoc

import cats.effect.IO
import cats.implicits._
import com.phasmidsoftware.args.Args
import com.phasmidsoftware.core.FP.mapTryGuarded
import com.phasmidsoftware.core.Text.namesMatch
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

  System.err.println(s"KMLEditor: ${edits.mkString}") // TODO generate log message

  /**
   * Method to process the file defined by baseFilename by parsing it, editing it, and writing it out.
   * The input file extension is ".kml" and the output file suffix is "_out.kml"
   *
   * @param baseFilename the base filename.
   * @return an IO[Unit] which needs to be run.
   */
  def process(baseFilename: Try[String]): IO[Unit] = {
    System.err.println(s"KMLEditor.process $baseFilename") // TODO generate a log message
    val qsi: IO[Seq[Writer]] = for {
      w <- IO.fromTry(addExtension(baseFilename, "_out.kml"))
      f <- IO(new File(w))
      bW = new BufferedWriter(new FileWriter(f, false))
      ks <- KMLCompanion.loadKML(addExtension(baseFilename, ".kml"))
      ks2 = processKMLs(ks)
      ws <- renderKMLs(ks2, FormatXML(0))
      qs <- write(bW, ws).sequence
    } yield qs

    qsi map (qs => qs.reduce((q, _) => q)) flatMap (q => IO(q.close()))
  }

  /**
   * Method to process the given Placemark.
   *
   * @param p  the Placemark to process.
   * @param e  the edit which may (or may not) apply to <code>p</code>.
   * @param fs a sequence of Features which are the children of <code>p</code>'s family (including <code>p</code> itself).
   * @return an optional Feature.
   */
  def processPlacemark(p: Placemark, e: KmlEdit, fs: Seq[Feature]): Option[Option[Feature]] = e.operands match {
    case 1 => processPlacemark1(p, e)
    case 2 => processPlacemark2(p, e, fs)
  }

  /**
   * Method to process an object that has features.
   *
   * @param t the object to process.
   * @param g a function which takes a tuple of T and Seq[Feature] and creates an optional new copy of the T based on the given Feature sequence.
   * @tparam T the type of <code>t</code>.
   * @return an optional copy of <code>t</code> with a (potentially) new set of features.
   */
  def processHasFeatures[T](t: T)(g: (T, Seq[Feature]) => Option[T]): Option[T] = t match {
    case h: HasFeatures => g(t, processFeatures(h.features))
    case _ => throw new Exception(s"processHasFeatures: parameter t does not extend HasFeatures: ${t.getClass}")
  }

  /**
   * Process the given feature set and return a new feature set that is the result of the processing.
   *
   * @param fs a sequence of Feature, the children of a particular object that extends HasFeatures (i.e. Kml, Document, or Folder).
   * @return a (potentially) different sequence of Feature.
   */
  def processFeatures(fs: Seq[Feature]): Seq[Feature] = for (f <- fs; z <- processFeature(f, fs)) yield z

  /**
   * Method to process a sequence of KML objects.
   * In practice, there is only ever one such object in a KML file.
   *
   * @param ks a sequence of KML.
   * @return a sequence of KML, quite possibly different from the input sequence.
   */
  def processKMLs(ks: Seq[KML]): Seq[KML] = for (k <- ks; z <- processHasFeatures(k)((t, fs) => Some(t.copy(features = fs)))) yield z

  /**
   * Method to join two Placemarks together.
   *
   * @param p    the Placemark
   * @param fs   the potential features to be joined with <code>p</code>. These are the siblings of <code>p</code> itself.
   * @param name the name of the feature to be joined, as defined by the edit.
   * @return an optional Feature which, if defined, is the new Placemark to be used instead of <code>p</code>.
   */
  def joinPlacemarks(p: Placemark, fs: Seq[Feature], name: String): Option[Feature] = {
    System.err.println(s"join: ${p.featureData.name} with $name") // TODO generate a log message
    val zz = for (f <- fs if f != p) yield joinPlacemarks(p, name, f)
    for (z <- zz.find(_.isDefined); q <- z) yield q
  }

  /**
   * CONSIDER why do we not define mergeable Geometry?
   *
   * @param gp Geometry from p.
   * @param gq Geometry from q.
   * @return Option[LineString].
   */
  def mergeLineStrings(gp: Geometry, gq: Geometry): Option[LineString] = (gp, gq) match {
    case (lp: LineString, lq: LineString) => lp merge lq
    case _ => None
  }

  /**
   * Method to process the given Placemark with zero additional Features.
   *
   * @param p the Placemark to process. Theoretically, <code>p</code> could be some other type of Feature.
   * @param e the edit which may (or may not) apply to <code>p</code>.
   * @return an optional Feature.
   */
  private def processPlacemark1(p: Placemark, e: KmlEdit): Option[Option[Feature]] = (p.featureData.name, e) match {
    case (name, KmlEdit(KmlEdit.DELETE, _, Element(_, name1), None))
      if namesMatch(name, name1) =>
      System.err.println(s"delete: ${p.featureData.name}") // TODO generate a log message
      Some(None)
    case (_, KmlEdit(KmlEdit.DELETE, _, _, _)) =>
      Some(Some(p))
    case _ =>
      None
  }

  /**
   * Method to process the given Placemark with one additional Features.
   *
   * @param p  the Placemark to process.
   * @param e  the edit which may (or may not) apply to <code>p</code>.
   * @param fs a sequence of Features which are the children of <code>p</code>'s family (including <code>p</code> itself).
   * @return an optional optional Feature.
   */
  private def processPlacemark2(p: Placemark, e: KmlEdit, fs: Seq[Feature]): Option[Option[Feature]] =
    (p.featureData.name, e) match {
      case (name, KmlEdit(KmlEdit.JOIN, _, Element("Placemark", name1), Some(Element("Placemark", name2))))
        if namesMatch(name, name1) =>
        Some(joinPlacemarks(p, fs, name2))
      case _ =>
        None
    }

  private def joinPlacemarks(p: Placemark, name: String, feature: Feature): Option[Feature] =
    feature match {
      case q: Placemark if namesMatch(q.featureData.name, name) => joinPlacemarks(p, q)
      case _ => None
    }

  /**
   * NOTE: we should implement this by making Placemark extend Mergeable.
   *
   * @param p the first Placemark.
   * @param q the second Placemark.
   * @return an Option[Placemark].
   */
  private def joinPlacemarks(p: Placemark, q: Placemark): Option[Placemark] = {
    println(s"joinPlacemarks: ${p.featureData.name},  ${q.featureData.name}")
    val gps: Seq[Geometry] = p.Geometry
    val gqs = q.Geometry
    val los: Seq[Option[LineString]] = for (gp <- gps; gq <- gqs) yield mergeLineStrings(gp, gq)
    val z: Seq[LineString] = los filter (_.isDefined) map (_.get)
    for {
      xx <- p.featureData merge q.featureData
    } yield Placemark(z)(xx)
  }

  private def processFeature(f: Feature, fs: Seq[Feature]): Option[Feature] =
    f match {
      case p: Placemark =>
        // XXX we create a list (foos) of optional features, each element of the list arising from a particular edit. There should be at most one defined result.
        val foos = for (e <- edits) yield processPlacemark(p, e, fs)
        // XXX we create a list (xs) of feature(s) corresponding to the defined results in foos.
        val xs = foos.filter(_.isDefined) map (_.get)
        // XXX if xs is not empty, we return its head, otherwise we return f.
        if (xs.nonEmpty) xs.head else Some(f)
      case d: Document =>
        processHasFeatures(d)((t, fs) => Some(t.copy(features = fs)(d.containerData)))
      case x: Folder =>
        processHasFeatures(x)((t, fs) => Some(t.copy(features = fs)(x.containerData)))
      case _ => Some(f) // Container
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

  private def addExt(basename: String, ext: String): String = basename + ext
}
