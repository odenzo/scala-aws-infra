package com.odenzo.utils

import cats._
import cats.data._
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._
import fs2._
import fs2.interop.reactivestreams._
import org.reactivestreams.Publisher

import scala.concurrent.duration.FiniteDuration

case class RetryableError(msg: String) extends Throwable(msg) {
  val retry = true
}

/** Some FS2 Utils, retry utilities and  Reactive Stream bridges. All are lazy so should rationalize */
object FS2Utils {

  /** Consumes the whole stream as a list, for paginator. This is lazy. */
  def toList[T, U](publisher: => Publisher[T])(implicit cs: ContextShift[IO]): IO[List[T]] = publisher.toStream[IO].compile.toList

  /** Takes a tunk and suspends running it. Usually we are already in an IO but.... */
  def toStream[T](publisher: => Publisher[T])(implicit cs: ContextShift[IO]): IO[Stream[IO, T]] = {
    IO(publisher).map(_.toStream[IO])
  }

  /** Creates a stream of U for when publisher returns a List like structure of items under the main response T */
  def toBurstStream[T, U](publisher: => Publisher[T])(fn: T => IterableOnce[U])(implicit cs: ContextShift[IO]): IO[Stream[IO, U]] = {
    toStream(publisher).map { s => s.map(t => fn(t)).flatMap(u => fs2.Stream.emits(u.iterator.toSeq)) }
  }

  def toBurstList[T, U](publisher: => Publisher[T])(fn: T => IterableOnce[U])(implicit cs: ContextShift[IO]): IO[List[U]] = {
    toBurstStream(publisher)(fn).flatMap(_.compile.toList)
  }

  def burst[A, B](fn: A => IterableOnce[B])(stream: Stream[IO, A]): Stream[IO, B] = {
    val unwrapped: Stream[IO, IterableOnce[B]] = stream.map(fn(_))
    val burst: Stream[IO, B]                   = unwrapped.flatMap((uw: IterableOnce[B]) => fs2.Stream.emits(uw.iterator.toSeq))
    burst
  }

  /** This will retry, so take care to align your indempotency tokens as needed.
    * fn is basically checking for a condition and return A or raising a RetryableError
    * Other exceptions are passed through unchanged
    */
  def uniformRetry[A](interval: FiniteDuration, maxAttempts: Int)(fn: => IO[A])(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[A] = {
    def retryIf(e: Throwable) = e.isInstanceOf[RetryableError]
    Stream.retry(fn, interval, identity, maxAttempts, retryIf).compile.lastOrError
  }

  def backoffRetry[A](delay: FiniteDuration, maxAttempts: Int)(fn: => IO[A])(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[A] = {
    val backoffFunc: FiniteDuration => FiniteDuration = (d: FiniteDuration) => d * 2
    def retryIf(e: Throwable)                         = e.isInstanceOf[RetryableError]
    Stream.retry(fn, delay, backoffFunc, maxAttempts, retryIf).compile.lastOrError
  }

}
