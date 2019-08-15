package com.sandbox.c2


trait Semigroup[A] {

  def combine(a: A, b: A): A

}

trait MonoidSupport[A] extends Semigroup[A] {

  def empty: A

}

object BooleanInstances {

  implicit val And = new MonoidSupport[Boolean] {

    override def empty: Boolean = true

    override def combine(a: Boolean, b: Boolean): Boolean = a && b
  }

  implicit val Or = new MonoidSupport[Boolean] {

    override def empty: Boolean = false

    override def combine(a: Boolean, b: Boolean): Boolean = a || b

  }

  implicit val Either = new MonoidSupport[Boolean] {

    override def empty: Boolean = true

    override def combine(a: Boolean, b: Boolean): Boolean = {
      (!a && b) || (a && !b)
    }

  }

  implicit val Xor = new MonoidSupport[Boolean] {

    override def empty: Boolean = false

    override def combine(a: Boolean, b: Boolean): Boolean = {
      (!a || b) && (a || !b)
    }

  }


}

object MonoidSupport {

  def apply[A](implicit monoid: MonoidSupport[A]): MonoidSupport[A] = monoid


  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: MonoidSupport[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)
                    (implicit m: MonoidSupport[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

}

object AndTest extends App {

  import BooleanInstances.And

  val and = MonoidSupport[Boolean]

  assert(and.combine(true, false) == false)
  assert(and.combine(true, true) == true)
  assert(and.combine(false, false) == false)


  assert(and.combine(and.combine(true, true), true))


}
