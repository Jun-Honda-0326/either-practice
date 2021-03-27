package exmaple

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

object FutureEitherExample {

  def main(args: Array[String]): Unit = {
    divideBy2AsyncEither(4)
    divideBy2AsyncEither(3)
  }

  import EitherExample.divideEither
  import DivideError._

  def divideAsyncEither(num: Int, demon: Int): Future[Either[DivideError, Int]] = {
    Future(divideEither(num, demon))
  }

  def divideBy2AsyncEither(num: Int): Unit = {
    val result: Future[Either[DivideError, Int]]
      = for {
        res1 <- divideAsyncEither(num, 2)
        re2  <- divideAsyncEither(num, 2)
        res3 <- divideAsyncEither(num, 2)
      } yield res3
      result.onComplete {
        case Success(value) => println(value)
        case Failure(e)     => println(e)
      }
    }


  }
