import scala.concurrent.{ Future, ExecutionContext }
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.EitherT
import cats.implicits._

import java.time.LocalDate

object  EitherTPractice {

  case class Worker(name: String)

  trait FetchWorkerOnWorkDay {
    import FetchWorkerOnWorkDay._
    def excute(date: LocalDate): Future[Either[Err, List[Worker]]]
  }
  object FetchWorkerOnWorkDay {
    sealed trait Err
    object Err {
      case object NoWorkers extends Err
    }
  }

  trait FetchCompanyRequest {
    import FetchCompanyRequest._
    def excute(companyId: Long, date: LocalDate): Future[Either[Err, CompanyRequest]]
  }
  case class CompanyRequest(companyId: Long, date: LocalDate, numberOfPeople: Int, content: String)
  object FetchCompanyRequest {
    sealed trait Err
    object Err {
      case object CompanyNotFound extends Err
      case object IllegalDate extends Err
      case object NotRecruiting extends Err
    }
  }

  class WorkerAssignToCompanyService(
    fetchWorkersOnDay: FetchWorkerOnWorkDay,
    fetchCompanyRequest: FetchCompanyRequest
  ) {
    import WorkerAssignToCompanyService._
    def excute(date: LocalDate, companyId: Long): Future[Either[Err, Result]] = {
      val e: EitherT[Future, Err, Result] = for {
        companyRequest <- EitherT(fetchCompanyRequest.excute(companyId, date)).leftMap {
          case FetchCompanyRequest.Err.CompanyNotFound => Err.CompanyNotFound
          case FetchCompanyRequest.Err.IllegalDate     => Err.IllegalDate
          case FetchCompanyRequest.Err.NotRecruiting   => Err.NotRecruiting
        }
        workersOnWorkDay <- EitherT(fetchWorkersOnDay.excute(date)).leftMap {
          case FetchWorkerOnWorkDay.Err.NoWorkers => Err.NoWorkers
        }
        assignedWorkers <- EitherT(Future.successful(assignWorkers(workersOnWorkDay, companyRequest.numberOfPeople)))
      } yield Result(companyRequest, assignedWorkers)
      e.value
    }

    def assignWorkers(workers: List[Worker], numberOfPeople: Int): Either[Err, List[Worker]] = {
      if (workers.length < numberOfPeople) Left(Err.NotEnoughWorkers)
      else Right(workers.take(numberOfPeople))
    }

  }


  object WorkerAssignToCompanyService {
    case class Result(companyRequest: CompanyRequest, workers: List[Worker])
    sealed trait Err
    object Err {
      case object CompanyNotFound extends Err
      case object IllegalDate extends Err
      case object NotRecruiting extends Err
      case object NoWorkers extends Err
      case object NotEnoughWorkers extends Err
    }
  }

  trait Foo
  sealed trait FooErr
  object FooErr extends FooErr
  trait FooRepository {
    def findBy(fooId: Long): Future[Either[FooErr, Foo]]
  }

  trait Bar
  sealed trait BarErr
  object  BarErr extends BarErr
  trait  BarRepository {
    def findBy(BarId: Long): Future[Either[BarErr, Bar]]
  }

  case class FooBar(foo: Foo, bar: Bar)
  sealed trait FooBarErr
  object FooBarErr extends FooBarErr
  class FooBarService(
    fooRepository: FooRepository,
    barRepository: BarRepository
  ){
    def findFooBar(fooId: Long, barId: Long): Future[Either[FooBarErr, FooBar]] = {
      val e = for {
        foo <- EitherT(fooRepository.findBy(fooId)).leftMap {
          case FooErr => FooBarErr
        }
        bar <- EitherT(barRepository.findBy(barId)).leftMap {
          case BarErr => FooBarErr
        }
      } yield FooBar(foo, bar)
      e.value
    }
  }

  trait hogfe {
    def a: Future[Option[Int]]
    def b: Future[Either[String, Int]] = a.map(_.toRight("left"))
  }

}
