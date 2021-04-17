package example

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

class EitherExampleSpec extends WordSpec with Matchers {

  import EitherExample._
  import DivideError._

  "divideBy2Either" should {

    "do once" in {
      val num = 12
      val result = Right(6)

      divideBy2Either(num) shouldEqual result
    }

    "do twice usint flatMap" in {
      val num = 12
      val result = Right(3)
      val composed: Either[DivideError, Int] = divideBy2Either(num).flatMap {
        res1 => divideBy2Either(res1)
      }
      composed shouldEqual result
    }

    "do three times using flatMap" in {
      val num = 12
      val result = Left(Indivisible(3, 2))
      val composed: Either[DivideError, Int] = divideBy2Either(num).flatMap {
        res1 => divideBy2Either(res1).flatMap {
          res2 => divideBy2Either(res2)
        }
      }
      composed shouldEqual result
    }

  }

  "divideEither" should {

    "do once" in {
      val num  = 12
      val demon = 2
      val result = Right(6)

      divideEither(num, demon) shouldEqual result
    }

    "do once zero dividsion" in {
      val num = 12
      val demon = 0
      val result = Left(ZeroDivision)

      divideEither(num, demon) shouldEqual result
    }

  }

}
