package com.phasmid.laScala.fp

/**
  * Type class trait to define something that has a key.
  *
  * TODO try to generalize this so that return value implements Ordering, rather than being any particular type like String.
  *
  * Created by scalaprof on 11/17/16.
  */
trait HasStringKey[V] {
    def getKey(v: V): String
}
