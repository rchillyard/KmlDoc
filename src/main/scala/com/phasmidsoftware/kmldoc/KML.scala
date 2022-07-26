package com.phasmidsoftware.kmldoc

import java.net.URL
import scala.xml.{Elem, Node, NodeSeq, XML}

case class KML()

case class Document(name: String, description: String)

object Document {
  def fromXML(node: Node): NodeSeq = node \ "Document"
}

object KML {
  def loadKML(resource: URL): KML = {
    require(resource != null)
    loadKML(resource.getPath)
  }

  def loadKML(file: String): KML = {
    require(file != null)
    val xml: Elem = XML.loadFile(file)
    val kmls: collection.Seq[NodeSeq] = for (kml <- xml \\ "kml") yield Document.fromXML(kml)
    println(kmls.size)
    val kml = kmls.headOption
    println(kml)
    KML()
  }

}

object Test extends App {
  KML.loadKML(KML.getClass.getResource("sample.kml"))
}