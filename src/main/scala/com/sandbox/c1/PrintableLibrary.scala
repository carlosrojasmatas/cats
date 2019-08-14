package com.sandbox.c1

import com.sandbox.c1.PrintableLibrary.Printable

object PrintableLibrary {

  trait Printable[A] {
    def format(obj: A): String
  }

}


object Cat {

  implicit val printableCat = new Printable[Cat] {
    override def format(obj: Cat): String = s"${obj.name} is a ${obj.age} old ${obj.color} cat"
  }

}

final case class Cat(name: String, age: Int, color: String)


object PrintableInstances {

  implicit val printableInt = new Printable[Int] {
    override def format(obj: Int): String = obj.toString
  }

  implicit val printableString = new Printable[String] {
    override def format(obj: String): String = obj
  }


}

object Printable {

  def format[A](obj: A)(implicit printable: Printable[A]): String = {
    printable.format(obj)
  }

  def print[A](obj: A)(implicit printable: Printable[A]) = {
    println(format(obj))
  }
}

object PrintableSyntax {

  implicit class PrintableOps[A](obj: A) {

    def format(implicit printer: Printable[A]): String = printer.format(obj)

    def print(implicit printer: Printable[A]) = println(format(printer))
  }

}

object PrinterWithSyntax extends App {

  import PrintableSyntax._
  import PrintableInstances._

  val s = "hello"
  val i = 1234
  val c = Cat("rocky", 7, "black")

  println(i.format)
  println(c.format)
  s.print
  i.print
  c.print
}

object Printer extends App {

  import PrintableInstances._

  val s1 = "Hello world"
  val s2 = "Hello again"
  val i1 = 1234
  val i2 = 456

  val f1 = Printable.format(s1)
  val f2 = Printable.format(s2)
  val f3 = Printable.format(i1)
  val f4 = Printable.format(i2)

  println(f1)
  println(f2)
  println(f3)
  println(f4)

  Printable.print(s1)
  Printable.print(s2)
  Printable.print(i1)
  Printable.print(i2)

  val c = Cat("pocho", 8, "red")

  Printable.print(c)
}

