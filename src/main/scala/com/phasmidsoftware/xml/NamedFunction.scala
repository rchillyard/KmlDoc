package com.phasmidsoftware.xml

import com.phasmidsoftware.kmldoc.KmlRenderers
import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * Trait to allow for functions to have names.
 * A function, for example T=>R, can simply extend NamedFunction[T=>R] and toString will show the name.
 *
 * The name is held as a var. It is updated by the ^^ method.
 *
 * CONSIDER having a method which adds to the name but doesn't replace it.
 *
 * @tparam E the type of the function to be named.
 */
trait NamedFunction[E] {
    def ^^(w: String): E = {
        name = w
        this.asInstanceOf[E]
    }

    def ^+(w: String): E = {
        name = s"$w: $name"
        this.asInstanceOf[E]
    }

    var name: String = "unnamed"

    override def toString: String = name
}

object NamedFunction {
    def assertNamedNotNullMember[N: NamedFunction, T: ClassTag](member: String): Unit = {
        Option(implicitly[NamedFunction[N]]) match {
            case None =>
                val kml: KmlRenderers.type = KmlRenderers
                logger.warn(s"named function for $member of ${implicitly[ClassTag[T]]} is not initialized")
                throw new AssertionError(s"named function for $member of ${implicitly[ClassTag[T]]} is not initialized")
            case _ =>
        }
    }

    def assertNamedNotNull[N: NamedFunction, T: ClassTag](): Unit = assertNamedNotNullMember("anonymous")

    def named[P: NamedFunction]: NamedFunction[P] = implicitly[NamedFunction[P]]

    def name[P: NamedFunction]: String = Option(named[P]) map (_.name) getOrElse "uninitialized"

    def combineNamed2[T0: NamedFunction, T1: NamedFunction]: String =
        combineNames2(named[T0], named[T1])

    def combineNamed3[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction]: String =
        combineNames3(named[T0], named[T1], named[T2])

    def combineNamed4[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3: NamedFunction]: String =
        combineNames4(named[T0], named[T1], named[T2], named[T3])

    def combineNamed5[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3: NamedFunction, T4: NamedFunction]: String =
        combineNames5(named[T0], named[T1], named[T2], named[T3], named[T4])

    def combineNamed6[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3: NamedFunction, T4: NamedFunction, T5: NamedFunction]: String =
        combineNamed5[T0, T1, T2, T3, T4] + "+" + name[T5]

    def combineNamed7[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3: NamedFunction, T4: NamedFunction, T5: NamedFunction, T6: NamedFunction]: String =
        combineNamed6[T0, T1, T2, T3, T4, T5] + "+" + name[T6]

    def combineNameds2[T0: NamedFunction, T1](t1n: NamedFunction[T1]): String =
        combineNames2(named[T0], t1n)

    def combineNameds3[T0: NamedFunction, T1: NamedFunction, T2](t2n: NamedFunction[T2]): String =
        combineNames3(named[T0], named[T1], t2n)

    def combineNameds4[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3](t3n: NamedFunction[T3]): String =
        combineNames4(named[T0], named[T1], named[T2], t3n)

    def combineNameds5[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3: NamedFunction, T4](t4n: NamedFunction[T4]): String =
        combineNames5(named[T0], named[T1], named[T2], named[T3], t4n)

    def combineNameds6[T0: NamedFunction, T1: NamedFunction, T2: NamedFunction, T3: NamedFunction, T4: NamedFunction, T5](t5n: NamedFunction[T5]): String =
        combineNames6(named[T0], named[T1], named[T2], named[T3], named[T4], t5n)

    def combineNames2[T0, T1](t0n: NamedFunction[T0], t1n: NamedFunction[T1]): String =
        name(t0n) + "+" + name(t1n)

    def combineNames3[T0, T1, T2](t0n: NamedFunction[T0], t1n: NamedFunction[T1], t2n: NamedFunction[T2]): String =
        combineNames2(t0n, t1n) + "+" + name(t2n)

    def combineNames4[T0, T1, T2, T3](t0n: NamedFunction[T0], t1n: NamedFunction[T1], t2n: NamedFunction[T2], t3n: NamedFunction[T3]): String =
        combineNames3(t0n, t1n, t2n) + "+" + name(t3n)

    def combineNames5[T0, T1, T2, T3, T4](t0n: NamedFunction[T0], t1n: NamedFunction[T1], t2n: NamedFunction[T2], t3n: NamedFunction[T3], t4n: NamedFunction[T4]): String =
        combineNames4(t0n, t1n, t2n, t3n) + "+" + name(t4n)

    def combineNames6[T0, T1, T2, T3, T4, T5](t0n: NamedFunction[T0], t1n: NamedFunction[T1], t2n: NamedFunction[T2], t3n: NamedFunction[T3], t4n: NamedFunction[T4], t5n: NamedFunction[T5]): String =
        combineNames5(t0n, t1n, t2n, t3n, t4n) + "+" + name(t5n)
}
