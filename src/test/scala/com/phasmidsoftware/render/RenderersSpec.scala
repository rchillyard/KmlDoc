package com.phasmidsoftware.render

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RenderersSpec extends AnyFlatSpec with should.Matchers {

  behavior of "RenderersSpec"

  it should "renderer1" in {
    case class Greeting(greet: String)
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatFree(1)) shouldBe "{Hello}"
  }

  it should "renderer0" in {
    case object MyJunk
    object MyRenderers extends Renderers {
      implicit val rendererMyJunk: Renderable[MyJunk.type] = renderer0
    }
    import MyRenderers._
    rendererMyJunk.render(MyJunk, FormatFree(0)) shouldBe "{MyJunk}"
  }

  it should "intRenderer" in {
    new Renderers {}.intRenderer.render(1, FormatFree(0)) shouldBe "1"
  }

  it should "stringRenderer" in {
    new Renderers {}.stringRenderer.render("Hello", FormatFree(0)) shouldBe "Hello"
  }

  it should "booleanRenderer" in {
    new Renderers {}.booleanRenderer.render(true, FormatFree(0)) shouldBe "true"
  }

  it should "renderer2A" in {
    case class Complex(r: Double, i: Double)
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatFree(0)) shouldBe "{1.0, -1.0}"
  }

  it should "renderer2B" in {
    case class KV(k: String, v: Int)
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    KVRenderers.rendererKV.render(KV("a", -1), FormatFree(0)) shouldBe "{a, -1}"
  }

  it should "renderer4" in {
    case class KVVV(k: String, v: Int, b: Boolean, x: Double)
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVV("a", -1, b = false, math.Pi), FormatFree(0)) shouldBe "{a, -1, false, 3.141592653589793}"
  }

  it should "doubleRenderer" in {
    new Renderers {}.doubleRenderer.render(math.Pi, FormatFree(0)) shouldBe "3.141592653589793"
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    rendererIntSeq.render(Seq(42, 99, 1), FormatFree(0)) shouldBe
      """[42
        |99
        |1
        |]""".stripMargin
  }

  it should "renderer5" in {
    case class KVVVV(k: String, v: Int, b: Boolean, x: Double, l: Long)
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVVV] = renderer5(KVVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, b = false, math.Pi, 42L), FormatFree(0)) shouldBe "{a, -1, false, 3.141592653589793, 42}"
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    rendererIntOption.render(Some(42), FormatFree(0)) shouldBe "42"
  }

  it should "longRenderer" in {
    new Renderers {}.longRenderer.render(42L, FormatFree(0)) shouldBe "42"
  }

  it should "renderer3" in {
    case class KVV(k: String, v: Int, b: Boolean)
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    KVVRenderers.rendererKVV.render(KVV("a", -1, b = false), FormatFree(0)) shouldBe "{a, -1, false}"
  }

}
