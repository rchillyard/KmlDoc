//package com.phasmidsoftware.kmldoc
//
//import com.phasmidsoftware.xml.{Extractor, Extractors}
//import scala.util.Try
//import scala.xml.Elem
////
////trait Render[T] {
////    def render(t: T): String
////}
////
////object Render {
////    implicit object StyleSelector extends Render[StyleSelector] {
////        def render(t: StyleSelector): String = t match {
////            case s: Style1 => s"$t(${s.styleSelectorProperties})"
////            case s: Style2 => s"$t(${s.styleSelectorProperties})"
////        }
////    }
////}
//
//class KmlObject(_id: Int)
//
//class StyleSelector(_id: Int) extends KmlObject(_id)
//
//case class StyleSelectorProperties(_styleType: String, _id: Int)
//
//case class Style1($: String)(val styleSelectorProperties: StyleSelectorProperties) extends StyleSelector(styleSelectorProperties._id)
//
//case class Style2($: String)(val styleSelectorProperties: StyleSelectorProperties) extends StyleSelector(styleSelectorProperties._id)
//
//
//object KmlObjectExtractors extends Extractors {
//
//    import Extractors._
//
//    implicit val extractorStyleSelectorProperties: Extractor[StyleSelectorProperties] = extractor20(StyleSelectorProperties.apply)
//    implicit val extractorBT1: Extractor[StyleSelectorProperties => Style1] = extractorPartial10[String, StyleSelectorProperties, Style1](Style1.apply)
//    implicit val extractorBT2: Extractor[StyleSelectorProperties => Style2] = extractorPartial10[String, StyleSelectorProperties, Style2](Style2.apply)
//    implicit val extractorStyle1: Extractor[Style1] = extractorPartial[StyleSelectorProperties, Style1](extractorBT1)
//    implicit val extractorStyle2: Extractor[Style2] = extractorPartial[StyleSelectorProperties, Style2](extractorBT2)
//    implicit val extractorStyleSelector: Extractor[StyleSelector] = extractorAlt[StyleSelector, Style1, Style2]
//}
//
//object KmlObject extends App {
//
//    import KmlObjectExtractors._
//    import Render._
//
//    val style1: StyleSelector = Style1("Robin")(StyleSelectorProperties("myStyleType", 1))
//    println(implicitly[Render[StyleSelector]].render(style1))
//    val style2: StyleSelector = Style2("Robin")(StyleSelectorProperties("myStyleType", 2))
//    println(implicitly[Render[StyleSelector]].render(style2))
//    val xml: Elem = <style1 styleType="myStyleType" id="2">Robin</style1>
//    private val triedStyle: Try[StyleSelector] = implicitly[Extractor[Style1]].extract(xml)
//    triedStyle.foreach(s => println(implicitly[Render[StyleSelector]].render(s)))
//}