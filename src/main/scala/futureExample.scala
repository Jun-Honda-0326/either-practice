package exmaple

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

object FutureExample {

  def main(args: Array[String]): Unit = {
    divideBy2Async(4)
    divideBy2Async(3)
    divideBy2Async(0)
  }

  def divideAsync(num: Int, demon: Int): Future[Int] = Future {
    if (demon == 0) throw  new IllegalArgumentException("demon must not be 0")
    if (num % demon != 0 ) throw new Exception(s"$num is not divisible by $demon")
    (num / demon)
  }

  def divideBy2Async(num: Int): Unit = {
    val result: Future[Int] = for {
      res1 <- divideAsync(num, 2)
      res2 <- divideAsync(res1, 2)
      res3 <- divideAsync(res2, 2)
    } yield res3
    result.onComplete {
      case Success(v)  => println(v)
      case Failure(e)  => println(e)
    }

  }


}
