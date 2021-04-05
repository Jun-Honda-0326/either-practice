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
    fromAOrBToEitherT("Exception", 111)
    fromFAOrFBToEitherT(Some("Exception"), Some(222))
    fromFEitherToEitherT(Right(333) , List(Right(444)))
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

  def fromAOrBToEitherT(err: String, num: Int): Unit= {
    val numEitherT: EitherT[Option, String, Int] = EitherT.rightT(num)
    val errEitherT: EitherT[Option, String, Int] = EitherT.leftT(err)
    println(numEitherT)
    println(errEitherT)
  }

  def fromFAOrFBToEitherT(err: Option[String], num: Option[Int]): Unit = {
    val numEitherT: EitherT[Option, String, Int] = EitherT.right(num)
    val errEitherT: EitherT[Option, String, Int] = EitherT.left(err)
    println(numEitherT)
    println(errEitherT)
  }

  def fromFEitherToEitherT(either: Either[String, Int], listEither: List[Either[String, Int]]): Unit = {
    val numET: EitherT[List, String, Int] = EitherT.fromEither(either)
    val numFET: EitherT[List, String, Int] = EitherT(listEither)
    println(numET)
    println(listEither)

  }
}
