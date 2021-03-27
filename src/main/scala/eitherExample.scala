package example

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


object EitherExample {

  def main(args: Array[String]): Unit = {
    println(divideEither(6, 2))
    println(divideEither(6, 0))
    println(divideEither(1, 3))
    println(divideBy2Either(6))
  }

  import DivideError._

  def divideEither(num: Int, demon: Int): Either[DivideError, Int]  = {
    if (demon == 0) Left(ZeroDivision)
    else if (num % demon != 0) Left(Indivisible(num, demon))
    else Right(num / demon)
  }

  def divideBy2Either(num: Int): Either[DivideError, Int] = {
    for {
      res1 <- divideEither(num, 2)
      res2 <- divideEither(res1, 2)
      res3 <- divideEither(res2, 2)
    } yield res3
  }
}

  sealed trait DivideError
  object DivideError {
    case class Indivisible(num: Int, demon: Int) extends DivideError
    object ZeroDivision extends DivideError
  }
