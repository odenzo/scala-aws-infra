package com.odenzo.aws

import cats._
import cats.data._
import cats.effect.syntax.all._
import cats.effect.{IO, _}
import cats.syntax.all._
import com.odenzo.utils.OError
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.awscore.AwsResponse
import software.amazon.awssdk.awscore.exception.AwsServiceException
import software.amazon.awssdk.services.ec2.model.Ec2Exception

import scala.reflect.ClassTag

/** Some helpers to deal with AWS Errors in F when time to do it correctly. But not much consistency */
trait AwsErrorUtils {

  def resultSuccessful[T <: AwsResponse](msg: String = "Generic AWS Error")(rs: T) = {
    if (rs.sdkHttpResponse().isSuccessful) IO.pure(rs)
    else IO.raiseError(OError(msg))
  }

  /** For redeemeding AWS calls that have a  *nested* exception of given type, typically NotFoundException for the module */
  def reedemToOption[A, B: ClassTag](typ: B)(ioMaybe: IO[A])(implicit ct: ClassTag[B]): IO[Option[A]] = {
    ioMaybe.redeemWith(
      ex => {
        scribe.warn(s"Nested Cause ${ex.getCause}")
        scribe.warn(s"Type ${oprint(typ)}")
        if (ct.runtimeClass.isInstance(ex)) IO[Option[A]](None)
        else IO.raiseError(ex)
      },
      a => IO.pure(a.some)
    )
  }

  def nestedFn[A, T <: Throwable: ClassTag](ex: Throwable)(fn: PartialFunction[T, A]): Option[A] = {
    ex.getCause match {
      case t: T => fn.lift(t)
      case _    => Option.empty[A]
    }
  }

  /** Anyway, this is all I need for now.
    * Often for delete operations, we want to delete and ignore if not found.
    * Basically ignore one error code.
    * Usage: IO(doSomething)
    */
  def handleAwsCodes[A](codeToRes: (String, A)*)(e: Throwable): Option[A] = {
    e match {
      case err: AwsServiceException => codeToRes.toMap.get(err.awsErrorDetails.errorCode)
      case _                        => None
    }
  }

  /** Checks the nested throwable is of type T and fn is true on T.
    * If so, then returns Option.empty else rethrows
    */
  def recoverToOption[T <: Throwable: ClassTag](e: Throwable)(fn: T => Boolean): Option[Nothing] = {
    Option(e.getCause).fold(throw e) { // Cause can be null
      case nested: T => if (fn(nested)) Option.empty[Nothing] else throw e
      case _         => throw e
    }
  }

  /** If the throwables has a cause of type T return empy option, else rethrow e CAUTION: REFLECTION
    * Useful for redeem. But not a partial function so not with recover
    */
  def nestedRecoverToOption[T <: Throwable: ClassTag](e: Throwable): Option[Nothing] = {
    Option(e.getCause).fold(throw e) { // Cause can be null
      case _: T => Option.empty[Nothing]
      case _    => throw e
    }

  }

  /** Gets the nested exception returning None if not cause or not matched against T */
  def nestedException[T <: Throwable: ClassTag](e: Throwable) = {
    Option(e.getCause).flatMap {
      case nested: T => nested.some
      case _         => Option.empty
    }
  }

  /** If this is an Ec2Exception return the AwsErrorDetails or none */
  def ec2ErrorDetails(e: Throwable) = {
    nestedException[Ec2Exception](e).map(_.awsErrorDetails())
  }

  /** Partial function generator to use for recover */
  def ec2ErrorMatch(code: String)(e: Throwable) = {
    ec2ErrorDetails(e).flatMap { awsInfo =>
      if (awsInfo.errorCode.equals(code)) {
        scribe.debug(s"Matched EC2 $code to $awsInfo")
        awsInfo.some
      } else {
        scribe.info(s"Unmatched EC2 Exception $awsInfo")
        None
      }
    }
  }

  /** Generates a Partial Function to use in recover clauses that matches the code
    * to AWS EC2 Exceptions AwsErrorDetails, returning to if matches .
    * Add example code. recover gone away?
    */
  def recoverEc2ErrorCode[T](code: String, to: T): PartialFunction[Throwable, T] = {
    val fn: Throwable => Option[T] = ec2ErrorMatch(code)(_).map(_ => to)
    Function.unlift(fn)
  }

  /**  Revisit this, to try and catch some more wanky AWS Errors.
    * Also experiment with merging the stack traces and the Java CompletionException has nothing worthwhile
    *  catchNestedAwsError(IOU.toIO(client.call)) {
    *    case  ex: S3Exception => ...
    *  }
    * @param io
    * @param catcher
    * @tparam A
    * @tparam T
    * @return
    */
  def catchNestedAwsError[A, T <: Throwable: ClassTag](io: IO[A])(catcher: PartialFunction[Throwable, IO[A]]) = {
    io.handleErrorWith { err =>
      val handled: Option[IO[A]] = if (err.isInstanceOf[java.util.concurrent.CompletionException]) {
        catcher.lift(err.getCause)
      } else Option.empty[IO[A]]
      handled.getOrElse(IO.raiseError(err))
    }
  }

  /**
    * IO.delay(fn).handleErrorWith(awsNestedErrorHandler { case foo:SomeEx if bool => IO.raiseError or IO.pure(fo)
    * Like a partial redeem
    * @param err
    * @param catcher
    * @tparam A
    * @tparam T
    * @return
    */
  def awsNestedErrorHandler[A, T <: Throwable: ClassTag](catcher: PartialFunction[T, IO[A]])(err: T) = {

    val handled: Option[IO[A]] = {
      if (err.isInstanceOf[java.util.concurrent.CompletionException]) {
        nestedFn(err)(catcher)
      } else Option.empty[IO[A]]
    }
    handled.getOrElse(IO.raiseError(err))
  }
}

object AwsErrorUtils extends AwsErrorUtils

case class OAWSErr(msg: String) extends Throwable
