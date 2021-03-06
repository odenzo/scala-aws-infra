package com.odenzo.utils

import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._

import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/** Some utilities, these are for concrete IO type mostly, a few ApplicativeErrors */
trait IOU {

  def toIO[A](fn: => CompletableFuture[A]): IO[A] = IO.fromCompletableFuture(IO.delay(fn))

  /** Allows  toIO(Future.successful("a") or toIO(callReturningFuture(args))
    * such that the future is deflation because fn is call-by-name
    */
  def fromFuture[A](fn: => Future[A]): IO[A] = IO.fromFuture(IO(fn))

  /** Ensures list has exactly one element or raisesError */
  def exactlyOne[A](msg: String)(l: List[A]): IO[A] = {
    l match {
      case h :: Nil => IO.pure(h)
      case _        => IO.raiseError[A](OError(s"Requires List Size 1 but  ${l.length}: $msg"))
    }

  }

  /** Ensures 0 or 1 elements in a list, errors if > 1 */
  def optionOne[A](msg: String)(l: List[A]): IO[Option[A]] = {
    l match {
      case Nil      => IO.pure(Option.empty[A])
      case h :: Nil => IO.pure(h.some)
      case _        => IO.raiseError(OError(s"Requires List Size 0 or 1 but  ${l.length}: $msg"))
    }
  }

  /** Raises an Option.empty to Error */
  def required[A](msg: String)(o: Option[A]): IO[A] = {
    IO.fromOption(o)(OError(s"Option Value Required: $msg"))
  }

  /** Get Or ElseM */
  def whenEmpty[A](x: IO[A])(o: Option[A]): IO[A]                    = o.fold(x)(IO.pure)
  def whenDefined[A, T](fn: A => IO[T])(o: Option[A]): IO[Option[T]] = o.traverse(fn(_))

  def requireNone[A](msg: String)(o: Option[A]): IO[Unit] = {
    o match {
      case None    => IO.unit
      case Some(v) => IO.raiseError[Unit](OError(s"Option Value Must Be Empty:$msg ... \n $v  "))
    }
  }

  def nonEmptyListIO[A](msg: String)(l: List[A]): IO[NonEmptyList[A]] = {
    IO.fromOption(NonEmptyList.fromList(l))(OError(s"NEL Requires List of 1+ $msg"))
  }

  def wrapError[A](msg: String)(a: IO[A]): IO[A] = a.handleErrorWith(e => IO.raiseError(OError(msg, e)))

//  /** Be nice to have a macro do this for functions with auto message, new Cats Effect? */
//  def wrapAndTrace[F[_]: Monad, A](msg: String)(fn: F[A])(implicit F: Monad[F]): F[A] = {
//    for {
//      _    <- F.pure(scribe.info(s"******  Started $msg"))
//      res  <- fn.handleErrorWith { e =>
//                F.pure(scribe.warn(e)) *> cats.ApplicativeError[F, Throwable].raiseError(OError(msg, e))
//              }
//      done <- F.pure(scribe.info(s"******  Finished $msg"))
//    } yield res
//  }

  /** This executes fa once and then repeatadle executes fn until falses */
  def pollWhile[T](fa: IO[T])(fn: Unit => Boolean): IO[T] = {
    val loopTime = FiniteDuration(3, TimeUnit.SECONDS)
    fa.flatMap { t: T => IO.sleep(loopTime).iterateUntil(fn) *> IO.pure(t) }
  }

//  def pollWhileM[T](fa: IO[T])(fn: () => IO[Boolean]) = {
//    implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)
//    val loopTime                  = FiniteDuration(3, TimeUnit.SECONDS)
//    fa.flatMap { t: T => IO.iterateUntilM(IO.sleep(loopTime)).iterateUntilM(fn) *> IO.pure(t) }
//  }

//  def pollWhileA[T](fn: => IO[Boolean])(fa: IO[T]) = {
//    val loopTime = FiniteDuration(3, TimeUnit.SECONDS)
//
//    def tryAgain(lastVal: T): IO[T] =
//      fn.flatMap {
//        case true  =>
//          IO.sleep(loopTime)
//          fa.flatMap(tryAgain)
//        case false => IO.pure(lastVal)
//      }
//  }

}

object IOU extends IOU
