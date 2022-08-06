package com.phasmidsoftware.render

import com.phasmidsoftware.kmldoc.Scale
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RenderersSpec extends AnyFlatSpec with should.Matchers {

  case class Greeting($: String)

  case object MyJunk

  case class Complex(_r: Double, _i: Double)

  case class KV(_k: String, _v: Int)

  case class KVV(_k: String, _v: Int, _b: Boolean)

  case class KVVV(_k: String, _v: Int, _b: Boolean, _x: Double)

  case class KVVVV(_k: String, _v: Int, _b: Boolean, _x: Double, _l: Long)

  behavior of "Renderers (FormatText)"

  import Renderers._

  it should "renderer0" in {
    object MyRenderers extends Renderers {
      implicit val rendererMyJunk: Renderable[MyJunk.type] = renderer0
    }
    import MyRenderers._
    rendererMyJunk.render(MyJunk, FormatText(0), StateR()) shouldBe "{MyJunk}"
  }

  it should "renderer1" in {
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatText(1), StateR()) shouldBe "{Hello}"
  }

  it should "renderer2A" in {
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatText(0), StateR()) shouldBe """{ r="1.0", i="-1.0"}"""
  }

  it should "renderer2B" in {
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    KVRenderers.rendererKV.render(KV("a", -1), FormatText(0), StateR()) shouldBe """{ k="a", v="-1"}"""
  }

  it should "renderer4" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVV("a", -1, _b = false, math.Pi), FormatText(0), StateR()) shouldBe """{ k="a", v="-1", b="false", x="3.141592653589793"}"""
  }

  it should "intRenderer" in {
    Renderers.intRenderer.render(1, FormatText(0), StateR()) shouldBe "1"
  }

  it should "stringRenderer" in {
    Renderers.stringRenderer.render("Hello", FormatText(0), StateR()) shouldBe "Hello"
  }

  it should "booleanRenderer" in {
    Renderers.booleanRenderer.render(true, FormatText(0), StateR()) shouldBe "true"
  }

  it should "doubleRenderer" in {
    Renderers.doubleRenderer.render(math.Pi, FormatText(0), StateR()) shouldBe "3.141592653589793"
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    rendererIntSeq.render(Seq(42, 99, 1), FormatText(0), StateR()) shouldBe
      """[42
        |99
        |1
        |]""".stripMargin
  }

  it should "renderer5" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVVV] = renderer5(KVVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, _b = false, math.Pi, 42L), FormatText(0), StateR()) shouldBe """{ k="a", v="-1", b="false", x="3.141592653589793", l="42"}"""
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    rendererIntOption.render(Some(42), FormatText(0), StateR()) shouldBe "42"
  }

  it should "longRenderer" in {
    Renderers.longRenderer.render(42L, FormatText(0), StateR()) shouldBe "42"
  }

  it should "renderer3" in {
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    KVVRenderers.rendererKVV.render(KVV("a", -1, _b = false), FormatText(0), StateR()) shouldBe """{ k="a", v="-1", b="false"}"""
  }

  behavior of "Renderers (FormatXML)"

  it should "renderer1" in {
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatXML(0), StateR()) shouldBe "<Greeting>Hello</Greeting>"
  }

  it should "renderer1A" in {
    object MyRenderers extends Renderers {
      val renderer: Renderable[Scale] = renderer1(Scale)
    }
    MyRenderers.renderer.render(Scale(math.Pi), FormatXML(0), StateR()) shouldBe "<Scale>3.141592653589793</Scale>"
  }

  it should "renderer0" in {
    object MyRenderers extends Renderers {
      implicit val rendererMyJunk: Renderable[MyJunk.type] = renderer0
    }
    import MyRenderers._
    rendererMyJunk.render(MyJunk, FormatXML(0), StateR()) shouldBe "<MyJunk$>MyJunk</MyJunk$>"
  }

  it should "intRenderer" in {
    Renderers.intRenderer.render(1, FormatXML(0), StateR()) shouldBe "1"
  }

  it should "stringRenderer" in {
    Renderers.stringRenderer.render("Hello", FormatXML(0), StateR()) shouldBe "Hello"
  }

  it should "booleanRenderer" in {
    Renderers.booleanRenderer.render(true, FormatXML(0), StateR()) shouldBe "true"
  }

  it should "renderer2A" in {
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatXML(0), StateR()) shouldBe "<Complex r=\"1.0\" i=\"-1.0\"></Complex>"
  }

  it should "renderer2B" in {
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    KVRenderers.rendererKV.render(KV("a", -1), FormatXML(0), StateR()) shouldBe """<KV k="a" v="-1"></KV>"""
  }

  it should "renderer4" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVV("a", -1, _b = false, math.Pi), FormatXML(0), StateR()) shouldBe """<KVVV k="a" v="-1" b="false" x="3.141592653589793"></KVVV>"""
  }

  it should "doubleRenderer" in {
    Renderers.doubleRenderer.render(math.Pi, FormatXML(0), StateR()) shouldBe "3.141592653589793"
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    rendererIntSeq.render(Seq(42, 99, 1), FormatXML(0), StateR()) shouldBe
      """
        |42
        |99
        |1
        |
        |""".stripMargin
  }

  it should "renderer5" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVVV] = renderer5(KVVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, _b = false, math.Pi, 42L), FormatXML(0), StateR()) shouldBe """<KVVVV k="a" v="-1" b="false" x="3.141592653589793" l="42"></KVVVV>"""
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    rendererIntOption.render(Some(42), FormatXML(0), StateR()) shouldBe "42"
  }

  it should "longRenderer" in {
    Renderers.longRenderer.render(42L, FormatXML(0), StateR()) shouldBe "42"
  }

  it should "renderer3" in {
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    KVVRenderers.rendererKVV.render(KVV("a", -1, _b = false), FormatXML(0), StateR()) shouldBe """<KVV k="a" v="-1" b="false"></KVV>"""
  }
}
