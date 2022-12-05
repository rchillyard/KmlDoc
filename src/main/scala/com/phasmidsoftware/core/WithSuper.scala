package com.phasmidsoftware.core

/**
 * Trait which defines the behavior of a super-type in XML object terms (especially KML).
 * Note that both types are expected to extend Product, typically as case classes.
 *
 * @tparam T the type of the element.
 * @tparam S the type of its super-type.
 */
trait WithSuper[T <: Product, S <: Product] {
    val superObject: S
}

