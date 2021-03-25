package exmaple

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

object FutureEitherExample {

  def main(args: Array[String]): Unit = {
    println(divideBy2AsyncEither(4))
  }

  import EitherExample.divideEither
  import DivideError._

  def divideAsyncEither(num: Int, demon: Int): Future[Either[DivideError, Int]] = {
    Future(divideEither(num, demon))
  }

  def divideBy2AsyncEither(num: Int): Unit = {
    val result: Future[Either[DivideError, Int]]
    result.onComplete {
      case Success(value) => value match {
        case Left
      }
    }
    }

  }
