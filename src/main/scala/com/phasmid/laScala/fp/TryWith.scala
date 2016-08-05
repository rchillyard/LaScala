package com.phasmid.laScala.fp

/**
  * TryWith object based on: http://codereview.stackexchange.com/questions/79267/scala-trywith-that-closes-resources-automatically
  *
  * Created by scalaprof on 8/5/16.
  */
import java.io.Closeable
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

object TryWith {
  def apply[C <: Closeable, R](resource: => C)(f: C => R): Try[R] =
    Try(resource).flatMap(resourceInstance => {
      try {
        val returnValue = f(resourceInstance)
        Try(resourceInstance.close()).map(_ => returnValue)
      }
      catch {
        case NonFatal(exceptionInFunction) =>
          try {
            resourceInstance.close()
            Failure(exceptionInFunction)
          }
          catch {
            case NonFatal(exceptionInClose) =>
              exceptionInFunction.addSuppressed(exceptionInClose)
              Failure(exceptionInFunction)
          }
      }
    })
}
