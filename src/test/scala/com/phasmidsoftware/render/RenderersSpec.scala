package com.phasmidsoftware.render

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RenderersSpec extends AnyFlatSpec with should.Matchers {

  case class Greeting(greet: String)

  case object MyJunk

  case class Complex(r: Double, i: Double)

  case class KV(k: String, v: Int)

  case class KVV(k: String, v: Int, b: Boolean)

  case class KVVV(k: String, v: Int, b: Boolean, x: Double)

  case class KVVVV(k: String, v: Int, b: Boolean, x: Double, l: Long)

  behavior of "Renderers (FormatText)"

  it should "renderer1" in {
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatText(1)) shouldBe "{Hello}"
  }

  it should "renderer0" in {
    object MyRenderers extends Renderers {
      implicit val rendererMyJunk: Renderable[MyJunk.type] = renderer0
    }
    import MyRenderers._
    rendererMyJunk.render(MyJunk, FormatText(0)) shouldBe "{MyJunk}"
  }

  it should "intRenderer" in {
    new Renderers {}.intRenderer.render(1, FormatText(0)) shouldBe "1"
  }

  it should "stringRenderer" in {
    new Renderers {}.stringRenderer.render("Hello", FormatText(0)) shouldBe "Hello"
  }

  it should "booleanRenderer" in {
    new Renderers {}.booleanRenderer.render(true, FormatText(0)) shouldBe "true"
  }

  it should "renderer2A" in {
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatText(0)) shouldBe "{1.0, -1.0}"
  }

  it should "renderer2B" in {
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    KVRenderers.rendererKV.render(KV("a", -1), FormatText(0)) shouldBe "{a, -1}"
  }

  it should "renderer4" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVV("a", -1, b = false, math.Pi), FormatText(0)) shouldBe "{a, -1, false, 3.141592653589793}"
  }

  it should "doubleRenderer" in {
    new Renderers {}.doubleRenderer.render(math.Pi, FormatText(0)) shouldBe "3.141592653589793"
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    rendererIntSeq.render(Seq(42, 99, 1), FormatText(0)) shouldBe
      """[42
        |99
        |1
        |]""".stripMargin
  }

  it should "renderer5" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVVV] = renderer5(KVVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, b = false, math.Pi, 42L), FormatText(0)) shouldBe "{a, -1, false, 3.141592653589793, 42}"
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    rendererIntOption.render(Some(42), FormatText(0)) shouldBe "42"
  }

  it should "longRenderer" in {
    new Renderers {}.longRenderer.render(42L, FormatText(0)) shouldBe "42"
  }

  it should "renderer3" in {
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    KVVRenderers.rendererKVV.render(KVV("a", -1, b = false), FormatText(0)) shouldBe "{a, -1, false}"
  }

  behavior of "Renderers (FormatXML)"

  it should "renderer1" in {
    object MyRenderers extends Renderers {
      val rendererGreeting: Renderable[Greeting] = renderer1(Greeting)
    }
    MyRenderers.rendererGreeting.render(Greeting("Hello"), FormatXML(1)) shouldBe "<Greeting>Hello</Greeting>"
  }

  it should "renderer0" in {
    object MyRenderers extends Renderers {
      implicit val rendererMyJunk: Renderable[MyJunk.type] = renderer0
    }
    import MyRenderers._
    rendererMyJunk.render(MyJunk, FormatXML(0)) shouldBe "<MyJunk$>MyJunk</MyJunk$>"
  }

  it should "intRenderer" in {
    new Renderers {}.intRenderer.render(1, FormatXML(0)) shouldBe "1"
  }

  it should "stringRenderer" in {
    new Renderers {}.stringRenderer.render("Hello", FormatXML(0)) shouldBe "Hello"
  }

  it should "booleanRenderer" in {
    new Renderers {}.booleanRenderer.render(true, FormatXML(0)) shouldBe "true"
  }

  it should "renderer2A" in {
    object ComplexRenderers extends Renderers {
      val rendererComplex: Renderable[Complex] = renderer2(Complex)
    }
    ComplexRenderers.rendererComplex.render(Complex(1, -1), FormatXML(0)) shouldBe "<Complex>1.0, -1.0</Complex>"
  }

  it should "renderer2B" in {
    object KVRenderers extends Renderers {
      val rendererKV: Renderable[KV] = renderer2(KV)
    }
    KVRenderers.rendererKV.render(KV("a", -1), FormatXML(0)) shouldBe "<KV>a, -1</KV>"
  }

  it should "renderer4" in {
    object KVVVRenderers extends Renderers {
      val rendererKVVV: Renderable[KVVV] = renderer4(KVVV)
    }
    KVVVRenderers.rendererKVVV.render(KVVV("a", -1, b = false, math.Pi), FormatXML(0)) shouldBe "<KVVV>a, -1, false, 3.141592653589793</KVVV>"
  }

  it should "doubleRenderer" in {
    new Renderers {}.doubleRenderer.render(math.Pi, FormatXML(0)) shouldBe "3.141592653589793"
  }

  it should "sequenceRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntSeq: Renderable[Seq[Int]] = sequenceRenderer[Int]
    }
    import MyRenderers._
    rendererIntSeq.render(Seq(42, 99, 1), FormatXML(0)) shouldBe
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
    KVVVRenderers.rendererKVVV.render(KVVVV("a", -1, b = false, math.Pi, 42L), FormatXML(0)) shouldBe "<KVVVV>a, -1, false, 3.141592653589793, 42</KVVVV>"
  }

  it should "optionRenderer" in {
    object MyRenderers extends Renderers {
      implicit val rendererIntOption: Renderable[Option[Int]] = optionRenderer[Int]
    }
    import MyRenderers._
    rendererIntOption.render(Some(42), FormatXML(0)) shouldBe "42"
  }

  it should "longRenderer" in {
    new Renderers {}.longRenderer.render(42L, FormatXML(0)) shouldBe "42"
  }

  it should "renderer3" in {
    object KVVRenderers extends Renderers {
      val rendererKVV: Renderable[KVV] = renderer3(KVV)
    }
    KVVRenderers.rendererKVV.render(KVV("a", -1, b = false), FormatXML(0)) shouldBe "<KVV>a, -1, false</KVV>"
  }

}
