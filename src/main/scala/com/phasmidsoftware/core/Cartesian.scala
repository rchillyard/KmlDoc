package com.phasmidsoftware.core

case class Cartesian(x: Double, y: Double, z: Double) {
  def vector(c: Cartesian): Cartesian = {
    val f = diff(c)(_)
    Cartesian(f(_.x), f(_.y), f(_.z))
  }

  def distance(c: Cartesian): Double = {
    val f = sqr(c)(_)
    math.sqrt(f(_.x) + f(_.y) + f(_.z))
  }

  def dotProduct(c: Cartesian): Double = {
    val f = mult(c)(_)
    f(_.x) + f(_.y) + f(_.z)
  }

  def sqr(c: Cartesian)(f: Cartesian => Double): Double = sqr(diff(c)(f)) //sqr(f(this) - f(c))

  def mult(c: Cartesian)(f: Cartesian => Double): Double = f(this) * f(c)

  def diff(c: Cartesian)(f: Cartesian => Double): Double = f(c) - f(this)

  def sqr(x: Double): Double = x * x
}
