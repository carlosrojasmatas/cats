package com.sandbox.c2
import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._
import cats.instances.option._
import com.sandbox.c2.AdderInstances.Order



object AdderInstances{

  case class Order(totalCost: Double, quantity: Double)

  implicit val OderAdder:Monoid[Order] = new Monoid[Order]{
    override def empty: Order = Order(0,0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost,x.quantity + y.quantity)
  }

}
class SuperAdder{

  def add[A](el:List[A])(implicit m:Monoid[A]):A = {
    el.foldLeft(m.empty)(_ |+| _)
  }
}

object Adder2 extends App {

  import AdderInstances._

  val sa = new SuperAdder
  println(sa.add(List(1,2,3)))
  println(sa.add(List(Some(1),Some(4),None)))

  val o1 = Order(1,2)
  val o2 = Order(3,4)
  val o3 = Order(5,6)

  println(sa.add(List(o1,o2,o3)))

}