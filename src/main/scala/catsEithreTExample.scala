import com.sun.net.httpserver.Authenticator.Failure

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}
import cats.data.EitherT
import cats.implicits._

object catsEithreTExample {

  def main(args: Array[String]): Unit = {
    println(Await.ready(divisionProgramAsync("10", "6").value, Duration.Inf))
    println(Await.ready(divisionProgramAsync("ok", "6").value, Duration.Inf))
  }

  def parseDouble(s: String): Either[String, Double] = {
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))
  }

  def divide(a: Double, b: Double): Either[String, Double] = {
    Either.cond(b != 0, a / b , "Cannot divide by zero")
  }

  def divideAsync(a: Double, b:Double): Future[Either[String, Double]] = {
    Future.successful(divide(a, b))
  }

  def parseDoubleAsync(s: String): Future[Either[String, Double]] = {
    Future.successful(parseDouble(s))
  }
//  EitherT[Future, String, Double]
  def divisionProgramAsync(inputA: String, inputB: String): EitherT[Future, String, Double] = {
    for {
      a <- EitherT(parseDoubleAsync(inputA))
      b <- EitherT(parseDoubleAsync(inputB))
      result <- EitherT(divideAsync(a, b))
    } yield result
  }

}
