package com.odenzo.utils

import cats._
import cats.data._
import cats.effect.tracing.PrintingOptions
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._

import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
trait IOU {

  def toIO[A](fn: => CompletableFuture[A])(implicit cs: ContextShift[IO]): IO[A] = {
    import scala.jdk.FutureConverters._
    val ff: IO[Future[A]] = IO.delay(fn.asScala)
    IO.fromFuture(ff) // Does IO.async under the hood.
  }

  /** Allows  toIO(Future.successful("a") or toIO(callReturningFuture(args))
    * such that the future is deflation because fn is call-by-name
    */
  def fromFuture[A](fn: => Future[A])(implicit cs: ContextShift[IO]): IO[A] = {
    IO.fromFuture(IO(fn))
  }

  /** Ensures list has exactly one element or raisesError */
  def exactlyOne[A](msg: String)(l: List[A]): IO[A] = {
    val err = OError(s"Requires List Size 1 but  ${l.length}: $msg")
    if (l.length > 1) IO.raiseError[A](err)
    else IO.fromOption(l.headOption)(err)
  }

  /** Ensures 0 or 1 elements in a list, errors if > 1 */
  def optionOne[A](msg: String)(l: List[A]): IO[Option[A]] = {
    val err = OError(s"Expected List Size 0 or 1 but  ${l.length}: $msg")
    if (l.length > 1) IO.raiseError[Option[A]](err)
    else IO.pure(l.headOption)
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

  /** Be nice to have a macro do this for functions with auto message, new Cats Effect? */
  def wrapAndTrace[A](msg: String)(fn: IO[A]): IO[A] = {
    val traceOptions = PrintingOptions.Default
      //.withShowFullStackTraces(true)
      .withMaxStackTraceLines(20)

    for {
      _   <- IO(scribe.info(s"******  Starting $msg"))
      res <- fn.handleErrorWith { e =>
               IO.trace
                 .map(trace => trace.showFiberTrace(traceOptions))
                 .flatMap(d => IO(scribe.warn(d))) *> IO.raiseError(OError(msg, e))
             }
      _   <- IO(scribe.info(s"******  Finished $msg"))
    } yield res
  }

  /** This executes fa once and then repeatadle executes fn until falses */
  def pollWhile[T](fa: IO[T])(fn: Unit => Boolean)(implicit cs: ContextShift[IO]): IO[T] = {
    implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)
    val loopTime                  = FiniteDuration(3, TimeUnit.SECONDS)
    fa.flatMap { t: T => IO.sleep(loopTime).iterateUntil(fn) *> IO.pure(t) }
  }

//  def pollWhileM[T](fa: IO[T])(fn: () => IO[Boolean])(implicit cs: ContextShift[IO]) = {
//    implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)
//    val loopTime                  = FiniteDuration(3, TimeUnit.SECONDS)
//    fa.flatMap { t: T => IO.iterateUntilM(IO.sleep(loopTime)).iterateUntilM(fn) *> IO.pure(t) }
//  }

//  def pollWhileA[T](fn: => IO[Boolean])(fa: IO[T])(implicit cs: ContextShift[IO]) = {
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
