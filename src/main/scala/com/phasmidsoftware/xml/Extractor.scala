package com.phasmidsoftware.xml

import scala.util.{Failure, Try}
import scala.xml.{Node, NodeSeq}

/**
 * Trait to define the behavior of a type which can be constructed from an XML Node.
 *
 * @tparam T the type to be constructed.
 */
trait Extractor[T] {
    self =>

    /**
     * Method to convert a Node into a Try[T].
     *
     * @param node a Node.
     * @return a Try[T].
     */
    def extract(node: Node): Try[T]

    /**
     * Method to create an Extractor[T] such that, if this Extractor[T] fails, then we invoke the (implicit) Extractor[P] instead.
     *
     * @tparam P the type of the alternative Extractor. P must provide implicit evidence of Extractor[P] and P must be a sub-class of T.
     * @return an Extractor[T].
     */
    def orElse[P <: T : Extractor](): Extractor[T] = (node: Node) => self.extract(node) orElse implicitly[Extractor[P]].mapTo[T].extract(node)

    /**
     * Method to create an Extractor[P] which instantiates a Try[T] but treats it as a Try[P] where P is a super-class of T.
     * CONSIDER why does this work when Extractor[T] is not defined with T as covariant?
     *
     * @tparam P the type of the Extractor we wish to return.
     * @return an Extractor[P].
     */
    def mapTo[P >: T]: Extractor[P] = (node: Node) => self.extract(node)
}

/**
 * Companion object to Extractor.
 */
object Extractor {
    /**
     * Method to create an Extractor[T] which always fails.
     *
     * @tparam T the underlying type of the result.
     * @return a failing Extractor[T].
     */
    def none[T]: Extractor[T] = (_: Node) => Failure(new NoSuchElementException)

    /**
     * Method (if needed) to uncurry a 6-level curried function.
     * Not used currently.
     *
     * @param f the original function.
     * @tparam T1 type of parameter 1.
     * @tparam T2 type of parameter 2.
     * @tparam T3 type of parameter 3.
     * @tparam T4 type of parameter 4.
     * @tparam T5 type of parameter 5.
     * @tparam T6 type of parameter 6.
     * @tparam R  the type of the result.
     * @return a function of type (T1, T2, T3, T4, T5, T6) => R
     */
    def uncurry6[T1, T2, T3, T4, T5, T6, R](f: T1 => T2 => T3 => T4 => T5 => T6 => R): (T1, T2, T3, T4, T5, T6) => R = (t1, t2, t3, t4, t5, t6) => f(t1)(t2)(t3)(t4)(t5)(t6)
}

/**
 * Trait to define the behavior of an iterable type which can be constructed from an XML NodeSeq.
 *
 * @tparam T the (iterable) type to be constructed.
 */
trait MultiExtractor[T] {
    /**
     * Method to convert a NodeSeq into a Try[T], usually an iterable type.
     *
     * @param nodeSeq a NodeSeq.
     * @return a Try[T].
     */
    def extract(nodeSeq: NodeSeq): Try[T]
}
