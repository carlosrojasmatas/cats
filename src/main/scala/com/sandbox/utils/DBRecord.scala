package com.sandbox.utils

object RecordTypes{

  trait Record {

    def get[A](field:String):A

  }


}


