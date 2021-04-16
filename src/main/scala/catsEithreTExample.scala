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
    eitherTFromOption()
    applicationErrorToEitherT()
    eitherTSemiFlatMapOption()
    println(Await.ready(eitherTSemiFlatMapFuture().value, Duration.Inf ))
    println(Await.ready(eitherTFor(), Duration.Inf))

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
    val numFE =  EitherT(Future(either))
    println(numET)
    println(listEither)
    println(numFE)
  }

  def eitherTFromOption():Unit = {
    val noneOpt: Option[Int] = None
    val listOpt: List[Option[Int]] = List(None, Some(2), Some(3), None, Some(4))
    val noneOptET: EitherT[Future, String, Int] = EitherT.fromOption[Future](noneOpt, "option not defined")
    println(noneOptET)
    val listOptET = EitherT.fromOptionF(listOpt, "option not defined")
    println(listOptET)
  }

  def applicationErrorToEitherT(): Unit = {
    val myTry: Try[Int] = Try(2)
    val myFuture: Future[String] = Future.failed(new Exception())

    val myTryET: EitherT[Try, Throwable, Int] = myTry.attemptT
    println(myTryET)
    val myFutureET: EitherT[Future, Throwable, String] = (myFuture.attemptT)
    println(myFutureET)
  }

  def eitherTSemiFlatMapOption(): Unit = {
    val either: Either[String, Int] = Right(10)
    val eitherT: EitherT[Option, String, Int] = EitherT.rightT(100)

    val eitherTOpt = eitherT semiflatMap {
      num => Some(99)
    }
    println(eitherTOpt)
  }

  def eitherTSemiFlatMapFuture(): EitherT[Future, String, Int]= {
    val either: Either[String, Int] = Right(10)
    val eitherT: EitherT[Future, String, Int] = EitherT.rightT(100)
      eitherT semiflatMap {
        num => Future.successful(999)
    }
  }

  def eitherTFor():Future[EitherT[List,String, Int]] = {
   val listEither = Future(List(Right(100), Right(200), Left("Not number"), Right(300), Left("Not number")))
   val  listEitherT = listEither.map(EitherT(_))
   for {
     eitherT <- listEitherT
   } yield  {
     println(eitherT)
     eitherT.semiflatMap(num => List(num + 100 ))
   }
  }



}
