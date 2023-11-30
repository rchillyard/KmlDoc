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

  println(s"KMLEditor: ${edits.mkString}")

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
      ks2 = processKMLs(ks)
      ws <- renderKMLs(ks2, FormatXML(0))
      qs <- write(bW, ws).sequence
    } yield qs

    qsi map (qs => qs.reduce((q, _) => q)) flatMap (q => IO(q.close()))
  }

  private def processKML(k: KML): KML = processHasFeatures(k)((t, fs) => t.copy(features = fs))
//  {
//    val fs = k.features
//    val fs_ = for (f <- fs) yield processFeature(f, fs)
//    k.copy(features = fs_)
//  }

  def processHasFeatures[T <: Product](t: T)(g: (T, Seq[Feature]) => T): T = t match {
    case h: HasFeatures =>
      val fs = h.features
      g(t, for (f <- fs) yield processFeature(f, fs))
    case _ => throw new Exception(s"processHasFeatures: parameter t does not extend HasFeatures: ${t.getClass}")
  }

  def processKMLs(ks: Seq[KML]): Seq[KML] = for (k <- ks) yield processKML(k)

  def deleteFeatureIfMatch(f: Feature, name: String): Option[Feature] = f match {
    case p: Placemark if p.featureData.name.toString == name => None
    case _ => Some(f)
  }

  private def joinPlacemarks(p: Placemark, features: Seq[Feature], name: String): Option[Feature] = {
    val zz = for (f <- features) yield joinPlacemarks(p, name, f)
    for (z <- zz.find(_.isDefined); q <- z) yield q
  }

  private def joinPlacemarks(p: Placemark, name: String, feature: Feature): Option[Feature] = feature match {
    case q: Placemark if p.featureData.name.$ == name => Some(joinPlacemarks(p, q))
    case _ => None
  }

  private def joinPlacemarks(p: Placemark, q: Placemark): Placemark = {
    println(s"joinPlacemarks: ${p.featureData.name},  ${q.featureData.name}")
    p
  } // FIXME to include q

  private def processPlacemark(p: Placemark, e: KmlEdit, features: Seq[Feature]): Option[Feature] =
    (p.featureData.name, e) match {
      case (name, KmlEdit(KmlEdit.JOIN, Element("Placemark", name1), Some(Element("Placemark", name2)))) if name.toString == name1 =>
        joinPlacemarks(p, features, name2)
      case (name, KmlEdit(KmlEdit.DELETE, Element(_, name1), None)) if name.toString == name1 =>
        None
      case (_, KmlEdit(KmlEdit.DELETE, _, _)) =>
        Some(p)
      case _ =>
        None
    }

  private def processFeature(f: Feature, fs: Seq[Feature]): Feature =
    f match {
      case p: Placemark =>
        val z: Seq[Option[Feature]] = for (e <- edits) yield processPlacemark(p, e, fs)
        val q = z.filter(_.isDefined) map (_.get)
        if (q.size == 1) q.head else p
      case d: Document =>
        processHasFeatures(d)((t, fs) => t.copy(features = fs)(d.containerData))
      case x: Folder =>
        processHasFeatures(x)((t, fs) => t.copy(features = fs)(x.containerData))

//      case d: Document =>
//        d.features
      case _ => f
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
