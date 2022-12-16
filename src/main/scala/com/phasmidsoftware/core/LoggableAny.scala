package com.phasmidsoftware.core

import com.phasmidsoftware.flog.Loggable

trait LoggableAny[T] extends Loggable[T] {
    def toLog(t: T): String = t.toString
}
