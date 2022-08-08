package com.phasmidsoftware.render

import com.phasmidsoftware.kmldoc.Scale
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Using}

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
    val wy = Using(StateR())(sr => rendererMyJunk.render(MyJunk, FormatText(0), sr))
    wy shouldBe Success("{MyJunk}")
  }

  it should "renderer1" in {
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    val wy = Using(StateR())(sr => MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatText(1), sr))
    wy shouldBe Success("{Hello}")
  }

  it should "renderer2A" in {
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    val wy = Using(StateR())(sr => ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatText(0), sr))
    wy shouldBe Success("""{ r="1.0" i="-1.0" }""")
  }

  it should "renderer2B" in {
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    val wy = Using(StateR())(sr => KVRenderers.rendererKV.render(KV("a", -1), FormatText(0), sr))
    wy shouldBe Success("""{ k="a" v="-1" }""")
  }

  it should "renderer4" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    val wy = Using(StateR())(sr => KVVVRenderers.rendererKVVV.render(KVVV("a", -1, _b = false, math.Pi), FormatText(0), sr))
    wy shouldBe Success("""{ k="a" v="-1" b="false" x="3.141592653589793" }""")
  }

  it should "intRenderer" in {
    val wy = Using(StateR())(sr => Renderers.intRenderer.render(1, FormatText(0), sr))
    wy shouldBe Success("1")
  }

  it should "stringRenderer" in {
    val wy = Using(StateR())(sr => Renderers.stringRenderer.render("Hello", FormatText(0), sr))
    wy shouldBe Success("Hello")
  }

  it should "booleanRenderer" in {
    val wy = Using(StateR())(sr => Renderers.booleanRenderer.render(true, FormatText(0), sr))
    wy shouldBe Success("true")
  }

  it should "doubleRenderer" in {
    val wy = Using(StateR())(sr => Renderers.doubleRenderer.render(math.Pi, FormatText(0), sr))
    wy shouldBe Success("3.141592653589793")
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    val wy = Using(StateR())(sr => rendererIntSeq.render(Seq(42, 99, 1), FormatText(0), sr))
    wy shouldBe
      Success(
        """[42
          |99
          |1
          |]""".stripMargin)
  }

  it should "renderer5" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVVV] = renderer5(KVVVV)
    }
    val wy = Using(StateR())(sr => KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, _b = false, math.Pi, 42L), FormatText(0), sr))
    wy shouldBe Success("""{ k="a" v="-1" b="false" x="3.141592653589793" l="42" }""")
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    val wy = Using(StateR())(sr => rendererIntOption.render(Some(42), FormatText(0), sr))
    wy shouldBe Success("42")
  }

  it should "longRenderer" in {
    val wy = Using(StateR())(sr => Renderers.longRenderer.render(42L, FormatText(0), sr))
    wy shouldBe Success("42")
  }

  it should "renderer3" in {
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    val wy = Using(StateR())(sr => KVVRenderers.rendererKVV.render(KVV("a", -1, _b = false), FormatText(0), sr))
    wy shouldBe Success("""{ k="a" v="-1" b="false" }""")
  }

  behavior of "Renderers (FormatXML)"

  it should "renderer1" in {
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    val wy = Using(StateR())(sr => MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatXML(0), sr))
    wy shouldBe Success("<Greeting>Hello</Greeting>")
  }

  it should "renderer1A" in {
    object MyRenderers extends Renderers {
      val renderer: Renderable[Scale] = renderer1(Scale)
    }
    val wy = Using(StateR())(sr => MyRenderers.renderer.render(Scale(math.Pi), FormatXML(0), sr))
    wy shouldBe Success("<Scale>3.141592653589793</Scale>")
  }

  it should "renderer0" in {
    object MyRenderers extends Renderers {
      implicit val rendererMyJunk: Renderable[MyJunk.type] = renderer0
    }
    import MyRenderers._
    val wy = Using(StateR())(sr => rendererMyJunk.render(MyJunk, FormatXML(0), sr))
    wy shouldBe Success("<MyJunk$>MyJunk</MyJunk$>")
  }

  it should "intRenderer" in {
    val wy = Using(StateR())(sr => Renderers.intRenderer.render(1, FormatXML(0), sr))
    wy shouldBe Success("1")
  }

  it should "stringRenderer" in {
    val wy = Using(StateR())(sr => Renderers.stringRenderer.render("Hello", FormatXML(0), sr))
    wy shouldBe Success("Hello")
  }

  it should "booleanRenderer" in {
    val wy = Using(StateR())(sr => Renderers.booleanRenderer.render(true, FormatXML(0), sr))
    wy shouldBe Success("true")
  }

  it should "renderer2A" in {
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    val wy = Using(StateR())(sr => ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatXML(0), sr))
    wy shouldBe Success("<Complex r=\"1.0\" i=\"-1.0\" ></Complex>")
  }

  it should "renderer2B" in {
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    val wy = Using(StateR())(sr => KVRenderers.rendererKV.render(KV("a", -1), FormatXML(0), sr))
    wy shouldBe Success("""<KV k="a" v="-1" ></KV>""")
  }

  it should "renderer4" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    val wy = Using(StateR())(sr => KVVVRenderers.rendererKVVV.render(KVVV("a", -1, _b = false, math.Pi), FormatXML(0), sr))
    wy shouldBe Success("""<KVVV k="a" v="-1" b="false" x="3.141592653589793" ></KVVV>""")
  }

  it should "doubleRenderer" in {
    val wy = Using(StateR())(sr => Renderers.doubleRenderer.render(math.Pi, FormatXML(0), sr))
    wy shouldBe Success("3.141592653589793")
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    val wy = Using(StateR())(sr => rendererIntSeq.render(Seq(42, 99, 1), FormatXML(0), sr))
    wy shouldBe Success(
      """
        |42
        |99
        |1
        |
        |""".stripMargin)
  }

  it should "renderer5" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVVV] = renderer5(KVVVV)
    }
    val wy = Using(StateR())(sr => KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, _b = false, math.Pi, 42L), FormatXML(0), sr))
    wy shouldBe Success("""<KVVVV k="a" v="-1" b="false" x="3.141592653589793" l="42" ></KVVVV>""")
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    val wy = Using(StateR())(sr => rendererIntOption.render(Some(42), FormatXML(0), sr))
    wy shouldBe Success("42")
  }

  it should "longRenderer" in {
    val wy = Using(StateR())(sr => Renderers.longRenderer.render(42L, FormatXML(0), sr))
    wy shouldBe Success("42")
  }

  it should "renderer3" in {
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    val wy = Using(StateR())(sr => KVVRenderers.rendererKVV.render(KVV("a", -1, _b = false), FormatXML(0), sr))
    wy shouldBe Success("""<KVV k="a" v="-1" b="false" ></KVV>""")
  }
}
