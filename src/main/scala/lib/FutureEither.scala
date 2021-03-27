package lib


import scala.util.{ Success, Failure }
import example.DivideError
import scala.concurrent.{ Future }
import scala.concurrent.ExecutionContext
import example.DivideError._
import example.FutureEitherExample

object FutureEitherExampleLib {
  def main(args: Array[String]): Unit = {
    import ExecutionContext.Implicits.global
    divideBy2AsyncFutureEither(4)
    divideBy2AsyncFutureEither(100)
    divideBy2AsyncFutureEither(999)
    divideBy2AsyncFutureEither(74398784)
  }

  case class FutureEither[A, B](value: Future[Either[A, B]]) {
    def map[BB](f: B => BB)(implicit ec: ExecutionContext): FutureEither[A, BB] = {
      FutureEither(value.map(_.map(f))(ec))
    }

    def flatMap[BB](f: B => FutureEither[A, BB])(implicit ec: ExecutionContext): FutureEither[A, BB] = {
      FutureEither {
        value.flatMap {
          case l @ Left(_) => Future.successful(l.asInstanceOf[Either[A, BB]])
          case Right(r) => f(r).value
        }
      }
    }
  }

  def divideBy2AsyncFutureEither(num: Int)(implicit ec: ExecutionContext): Unit = {
    val result: FutureEither[DivideError, Int] = for {
      res1 <- FutureEither(FutureEitherExample.divideAsyncEither(num, 2))
      res2 <- FutureEither(FutureEitherExample.divideAsyncEither(res1, 2))
      res3 <- FutureEither(FutureEitherExample.divideAsyncEither(res2, 2))
    } yield res3
    result.value.onComplete {
      case Success(v) => v match {
        case Right(num) => println(num)
        case Left(Indivisible(n ,d)) => println(s"$n is not divisible by $d")
        case Left(ZeroDivision)      => println("demon must not be 0")
      }
      case Failure(e) => e.printStackTrace()
    }
  }

}
