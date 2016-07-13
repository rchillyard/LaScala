package com.phasmid.laScala

import scala.concurrent.Future
import scala.util.Try

/**
  * Created by scalaprof on 7/12/16.
  */
package object cache {

  type FutureCache[K,V] = Cache[K,V,Future]

  type TryCache[K,V] = Cache[K,V,Try]

  type OptionCache[K,V] = Cache[K,V,Option]
}
