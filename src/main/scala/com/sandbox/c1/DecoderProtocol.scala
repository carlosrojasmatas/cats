package com.sandbox.c1

import com.sandbox.c1.DecoderProtocol.Decoder
import com.sandbox.c1.PersonDecoders.Person
import com.sandbox.utils.RecordTypes.Record

object DecoderProtocol {

  trait Decoder[A] {

    def decode(value: Record): A

  }


}

object PersonDecoders {

  final case class Person(id: String, name: String, lastName: String)

  implicit val personDecoder: Decoder[Person] = new Decoder[Person] {
    override def decode(value: Record): Person =
      Person(id = value.get("id"), name = value.get("name"), lastName = value.get("lastName"))
  }
}

object PersonProtocol {

  def fromRecord(r: Record)(implicit decoder: Decoder[Person]) = {
    decoder.decode(r)
  }

}

object PersonSyntax {

  implicit class RecordEnhancer(r: Record) {
    def asPerson: Person = {
      Person(
        id = r.get("id"),
        name = r.get("name"),
        lastName = r.get("lastName")
      )
    }
  }

}


object RecordTest extends App {

  import PersonSyntax._

//  implicit def some(a:Person):String = {
//    a.toString
//  }

  private final val data: Map[String, String] =
    Map(
      "id" -> "1234",
      "name" -> "carlos",
      "lastName" -> "rojas")

  val record = new Record {
    def get[A](field: String): A = data(field).asInstanceOf[A]
  }

//  val a: Person = PersonProtocol.fromRecord(record)
  val a:Person  = record.asPerson
  println(a.id)
}
