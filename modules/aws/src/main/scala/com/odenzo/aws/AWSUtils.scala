package com.odenzo.aws

import cats._
import cats.data._
import cats.effect.syntax.all._
import cats.effect.{ContextShift, IO, Timer, _}
import cats.syntax.all._
import fs2.Stream

import java.util.UUID
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.CollectionHasAsScala

// TODO: Move over IOU utils to here to prepare seperating out
// TODO: Cleanup duplicate functionallity waiters
trait AWSUtils {

  def scribeInfo(s: String): Unit = scribe.info(s)

  def completableFutureToIO[A](fn: => CompletableFuture[A])(implicit cs: ContextShift[IO]): IO[A] = {
    import scala.jdk.FutureConverters._
    val ff: IO[Future[A]] = IO.delay(fn.asScala)
    IO.fromFuture(ff) // Does IO.async under the hood.
  }

  /** Ensures list has exactly one element or raisesError */
  def exactlyOne[A](msg: String)(l: List[A]): IO[A] = {
    if (l.length > 1) IO.raiseError[A](OAWSErr(s"Requires List Size 1 but  ${l.length}: $msg"))
    else IO.fromOption(l.headOption)(OAWSErr(s"Requires List Size 1 but  ${l.length}: $msg"))
  }

  /** Ensures 0 or 1 elements in a list, errors if > 1 */
  def optionOne[A](msg: String)(l: List[A]): IO[Option[A]] = {
    val err = OAWSErr(s"Expected List Size 0 or 1 but  ${l.length}: $msg")
    if (l.length > 1) IO.raiseError[Option[A]](err)
    else IO.pure(l.headOption)
  }

  /** Raises an Option.empty to Error */
  def required[A](msg: String)(o: Option[A]): IO[A] = {
    IO.fromOption(o)(OAWSErr(s"Option Value Required: $msg"))
  }

  /** Convert a possible null Java list to a Scala list. */
  def nullsafeFromList[T](list: java.util.List[T]): List[T] = {
    if (list == null) scala.List.empty[T]
    else list.asScala.toList
  }

  def setTTL(): Unit = {
    java.security.Security.setProperty("networkaddress.cache.ttl", "60")
  }

  /** Some AWS Stuff follows the convention that field must consist of alphanumeric characters, '-', '_' or '.', and must start and end
    * with an alphanumeric character (e.g. 'MyName',  or 'my.name',  or '123-abc', regex used for validation is '([A-Za-z0-9][-A-Za-z0-9_.]*)?[A-Za-z0-9]')
    *
    * This is a crude sanitizer.
    */
  def sanitizeToDNS(s: String): IO[String] = {
    if (isDnsSanitized(s)) IO.pure(s)
    else {
      val clean = s.replace('/', '_') // Add more as needed
      if (isDnsSanitized(clean)) IO.pure(clean)
      else IO.raiseError(new OAWSErr(s"Could not sanitize $s"))
    }
  }

  def isDnsSanitized(s: String): Boolean = s.matches("(([A-Za-z0-9][-A-Za-z0-9_.]*)?[A-Za-z0-9])")

  def idempotentToken: String = UUID.randomUUID().toString.replace('-', 'z').take(32)

  /** Loops over check until it is either None or tst returns false.
    * Use case if checking if status of AWS resource is deleted. None if gone, else tst
    * - Trial semantics to see how it works.
    * - The looping is delayed by 10 seconds with a max of 15 tries
    */
  def waitWhile[A](tst: A => Boolean)(check: IO[Option[A]])(implicit cs: ContextShift[IO]): IO[Unit] = {
    val ec                        = scala.concurrent.ExecutionContext.global
    implicit val timer: Timer[IO] = IO.timer(ec)
    case class MarkedRetry(a: A) extends Throwable
    val afn = check.flatMap {
      case Some(v) => if (tst(v)) IO.raiseError(MarkedRetry(v)) else IO.unit
      case None    => IO.unit
    }

    // FS2 Retry I will steal for now, but it just reties on errors not success.
    def retryIf(e: Throwable) = e.isInstanceOf[MarkedRetry]
    val interval              = FiniteDuration(10, TimeUnit.SECONDS)
    val maxAttempts           = 15
    Stream.retry(afn, interval, identity, maxAttempts, retryIf).compile.lastOrError
  }

  /** Similar to waitWhile but used for checking when somethig ready, so it already exists
    * So the tst should be true when in desired state
    * @deprecated use checkUntil for better type infrerence
    */
  def waitUntil[A](tst: A => Boolean)(check: IO[A])(implicit cs: ContextShift[IO]): IO[Unit] = {
    val ec                        = scala.concurrent.ExecutionContext.global
    implicit val timer: Timer[IO] = IO.timer(ec)
    case class MarkedRetry(a: A) extends Throwable
    val afn = check.flatMap(v => if (!tst(v)) IO.raiseError(MarkedRetry(v)) else IO.unit)

    def retryIf(e: Throwable) = e.isInstanceOf[MarkedRetry]

    val interval    = FiniteDuration(10, TimeUnit.SECONDS)
    val maxAttempts = 15
    Stream.retry(afn, interval, identity, maxAttempts, retryIf).compile.lastOrError
  }

  /** This has better type inference since normally check is defined fn already */
  def checkUntil[A](check: IO[A])(tst: A => Boolean)(implicit cs: ContextShift[IO]): IO[Unit] = {
    val ec                        = scala.concurrent.ExecutionContext.global
    implicit val timer: Timer[IO] = IO.timer(ec)

    case class MarkedRetry(a: A) extends Throwable
    val afn                   = check.flatMap(v => if (!tst(v)) IO.raiseError(MarkedRetry(v)) else IO.unit)
    def retryIf(e: Throwable) = e.isInstanceOf[MarkedRetry]
    val interval              = FiniteDuration(10, TimeUnit.SECONDS)
    val maxAttempts           = 15
    Stream.retry(afn, interval, identity, maxAttempts, retryIf).compile.lastOrError
  }

  def repeatWhile[T](fn: Unit => Boolean)(fa: IO[T])(implicit cs: ContextShift[IO]): IO[T] = {
    implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)
    val loopTime                  = FiniteDuration(3, TimeUnit.SECONDS)
    fa.flatMap { t: T => IO.sleep(loopTime).iterateUntil(fn) *> IO.pure(t) }
  }

  /** Executed Polling function loop, delaying 3 seconds between.
    * Polling function returns T and the condition checks, if cond true keeps looping
    */
  def pollWhile[T](fn: T => Boolean)(pollFn: IO[T])(implicit cs: ContextShift[IO]): IO[T] = {
    implicit val timer: Timer[IO] = IO.timer(scala.concurrent.ExecutionContext.global)
    val loopTime                  = FiniteDuration(3, TimeUnit.SECONDS)
    val loopFn: IO[T]             = IO.sleep(loopTime) *> pollFn
    loopFn.iterateWhile(fn)
  }

}

object AWSUtils extends AWSUtils
