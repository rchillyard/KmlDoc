package com.phasmidsoftware.core

trait WithSuper[T <: Product, S <: Product] {
    val superObject: S
}

