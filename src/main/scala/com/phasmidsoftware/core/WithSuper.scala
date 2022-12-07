package com.phasmidsoftware.core

/**
 * Trait which defines the behavior of a super-type in XML object terms (especially KML).
 * Note that both types are typically as case classes, but this is not required.
 *
 * TODO: this can be removed
 *
 * @tparam T the type of the element.
 * @tparam S the type of its super-type.
 */
trait WithSuper[T, S] {
    val superObject: S
}

