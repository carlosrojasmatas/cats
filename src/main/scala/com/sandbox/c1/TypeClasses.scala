package com.sandbox.c1

import com.sandbox.c1.TypeClasses.{JsNull, JsObject, JsString, Json, JsonWriter}

object TypeClasses {


  sealed trait Json

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  case object JsNull extends Json


  trait JsonWriter[A] {
    def write(value: A): Json
  }

}

final case class P(name: String, email: String)

object JsonWriterInstances {

  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): TypeClasses.Json = JsString(value)
  }

  implicit val personWriter: JsonWriter[P] = new JsonWriter[P] {
    override def write(value: P): TypeClasses.Json =
      JsObject(Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      ))
  }

  implicit def optionWriter[A](implicit writer:JsonWriter[A]):JsonWriter[Option[A]] = {
    new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json = {
        value match {
          case Some(v) => writer.write(v)
          case _ => JsNull
        }
      }
    }
  }
}

object Json {

  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

object JsonSyntax {

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }

}

//object RecordTest extends App {
//  import JsonWriterInstances._
//
//  val a = implicitly[JsonWriter[String]]
//
//  println(a.write("gello"))
//}