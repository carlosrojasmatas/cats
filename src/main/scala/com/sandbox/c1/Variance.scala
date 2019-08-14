package com.sandbox.c1

import com.sandbox.c1.TypeClasses.{JsString, JsonWriter}

object Variance extends App {

  trait F[+A]

  trait B[-A]

  trait C[A]

  sealed trait Shape

  class Circle(radius: Int) extends Shape

  class Ellipsis(rad: Int, ang: Int) extends Circle(rad)

  class Rectangle(l1: Int, l2: Int) extends Shape


  //covariant in action
  val covShapeWrapper = new F[Shape] {}
  val covCircleWrapper = new F[Circle] {}
  val covRectWrapper = new F[Rectangle] {}
  val covEllipsisWrapper = new F[Ellipsis] {}

  val covariantShapeWriter: JsonWriter[F[Shape]] = new JsonWriter[F[Shape]] {
    override def write(value: F[Shape]): TypeClasses.Json = new JsString(value.toString)
  }

  covariantShapeWriter.write(covShapeWrapper)
  covariantShapeWriter.write(covCircleWrapper)
  covariantShapeWriter.write(covRectWrapper)

  val covariantCircleWriter: JsonWriter[F[Circle]] = new JsonWriter[F[Circle]] {
    override def write(value: F[Circle]): TypeClasses.Json = new JsString(value.toString)
  }

  covariantCircleWriter.write(covCircleWrapper)
  covariantCircleWriter.write(covEllipsisWrapper)
  /* Breaking the variance
    covariantCircleWriter.write(covShapeWrapper)
    covariantCircleWriter.write(covRectWrapper)
  */

  //contravariant in action

  val convShapeWrapper = new B[Shape] {}
  val convCircleWrapper = new B[Circle] {}
  val convRectWrapper = new B[Rectangle] {}
  val convEllipsisWrapper = new B[Ellipsis] {}


  val contravariantShapeWriter: JsonWriter[B[Shape]] = new JsonWriter[B[Shape]] {
    override def write(value: B[Shape]): TypeClasses.Json = new JsString(value.toString)
  }

  println(contravariantShapeWriter.write(convShapeWrapper))
  /* Breaking the contravariance
  contravariantShapeWriter.write(covariantCircleWriter)
  contravariantShapeWriter.write(convRectWrapper)
   */

  val contravariantCircleWriter: JsonWriter[B[Circle]] = new JsonWriter[B[Circle]] {
    override def write(value: B[Circle]): TypeClasses.Json = new JsString(value.toString)
  }

  println(contravariantCircleWriter.write(convShapeWrapper))
  println(contravariantCircleWriter.write(convCircleWrapper))
  /* Breaking the contravariance
  contravariantCircleWriter.write(convEllipsisWrapper)
   * */


  //invariant in action
  val invShapeWrapper = new C[Shape] {}
  val invCircleWrapper = new C[Circle] {}
  val invRectWrapper = new C[Rectangle] {}
  val invEllipsisWrapper = new C[Ellipsis] {}

  val invariantShapeWriter: JsonWriter[C[Shape]] = new JsonWriter[C[Shape]] {
    override def write(value: C[Shape]): TypeClasses.Json = new JsString(value.toString)
  }

  val invariantCircleWrapper: JsonWriter[C[Circle]] = new JsonWriter[C[Circle]] {
    override def write(value: C[Circle]): TypeClasses.Json = new JsString(value.toString)
  }


  println(invariantShapeWriter.write(invShapeWrapper))
  println(invariantCircleWrapper.write(invCircleWrapper))


  /*
  Breaking the invariance
  invariantShapeWriter.write(invCircleWrapper)
  invariantCircleWrapper.write(invShapeWrapper)
  invariantCircleWrapper.write(invEllipsisWrapper)
   */
}
