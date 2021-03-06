package com.phasmid.laScala.fp

/**
  * Type class trait to define something that has a key.
  *
  * CONSIDER try to generalize this so that return value implements Ordering, rather than being any particular type like String.
  *
  * Created by scalaprof on 11/17/16.
  */
trait HasKey[V] {
  type K

  /**
    * Get the key for value v as a String
    *
    * @param v a value
    * @return the "key" for that value
    */
  def getKey(v: V): K

  /**
    * The inverse of getKey. This allows us to form a value from a String
    *
    * @param w the key corresponding to the desired value
    * @return the value
    */
  def createValue(w: K): V
}
