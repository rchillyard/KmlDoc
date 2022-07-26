package com.phasmidsoftware.kmldoc

import com.phasmidsoftware.kmldoc.XmlUtils.maybeString

import java.net.URL
import scala.xml.{Elem, Node, NodeSeq, XML}

case class KML(xmlns: Option[String], documents: Seq[Document])

case class Document(name: String, description: Option[String])

object Document {
  def fromXML(node: Node): Document = {
    val name: NodeSeq = node \ "name"
    val description: NodeSeq = node \ "description"
    Document(maybeString(name).getOrElse("unnamed"), maybeString(description))
  }
}

object KML {
  def fromXML(node: Node): KML = {
    val xmlns: NodeSeq = node \ "@xmlns"
    val docs: NodeSeq = node \ "Document"
    KML(maybeString(xmlns), docs map (n => Document.fromXML(n)))
  }

  def loadKML(resource: URL): KML = {
    require(resource != null)
    loadKML(resource.getPath)
  }

  def loadKML(file: String): KML = {
    require(file != null)
    val xml: Elem = XML.loadFile(file)
    val kmls: collection.Seq[KML] = for (kml <- xml \\ "kml") yield KML.fromXML(kml)
    kmls.head
  }
}

object XmlUtils {
  def maybeString(ns: NodeSeq): Option[String] = ns.headOption.map(n => n.text)
}

object Test extends App {
  val kml: KML = KML.loadKML(KML.getClass.getResource("sample.kml"))
  println(s"KML: $kml")
}