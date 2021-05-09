package com.odenzo.aws

import cats._
import cats.effect.{IO, _}
import cats.syntax.all._
import com.odenzo.utils.OError
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.awscore.AwsResponse
import software.amazon.awssdk.awscore.exception.{AwsErrorDetails, AwsServiceException}

import scala.reflect.ClassTag

/** Some helpers to deal with AWS Errors in F when time to do it correctly. But not much consistency */
trait AwsErrorUtils {

  def narrowException[T <: Throwable: ClassTag](e: Throwable): Option[T] =
    e match {
      case e: T => e.some
      case _    => Option.empty[T]
    }

  /** Gets the nested exception returning None if not cause or not matched against T
    * Example:    nestedException[Ec2Exception](e)
    */
  def narrowNestedException[T <: Throwable: ClassTag](e: Throwable): Option[T] = {
    Option(e.getCause).flatMap(narrowException)
  }

  def resultSuccessful[T <: AwsResponse](msg: String = "Generic AWS Error")(rs: T): IO[T] = {
    if (rs.sdkHttpResponse().isSuccessful) IO.pure(rs)
    else IO.raiseError(OError(msg))
  }

  def checkError2[F[_]](x: Int)(implicit ae: ApplicativeError[F, IllegalArgumentException]): F[Int] =
    if (x < 0) ae.raiseError(new IllegalArgumentException("Foo")) else ae.pure(x)

  /** For redeemeding AWS calls that have a  *nested* exception of given type, typically NotFoundException for the module */
  def reedeemToOption[A, B: ClassTag](typ: B)(ioMaybe: IO[A])(implicit ct: ClassTag[B]): IO[Option[A]] = {
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

  /** If the nested cause is of type T then apply partial function fn.
    * @return None if no nested exception or doesn't match T or fn not covered
    */
  def nestedFn[A, T <: Throwable: ClassTag](ex: Throwable)(fn: PartialFunction[T, A]): Option[A] = {
    ex.getCause match {
      case t: T => fn.lift(t)
      case _    => Option.empty[A]
    }
  }

  /** This is a 'redeem' based on AWSServiceException Error Details Code.
    * Rather than a function if takes error code to A table. Assumes uniqueness and returns first match
    * e.g. narrowNestedException[AwsServiceException](e).flatMap(handleAwsCodes(("203","AThing"),("204","AnotherThing"))
    */
  def handleAwsCodes[A](codeToRes: (String, A)*)(e: AwsServiceException): Option[A] =
    codeToRes.find(_._1 == e.awsErrorDetails.errorCode).map(_._2)

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

  /** If this is an Ec2Exception return the AwsErrorDetails or none */
  def awsErrorDetails(e: Throwable): Option[AwsErrorDetails] = {
    narrowNestedException[AwsServiceException](e).map(_.awsErrorDetails())
  }

  /** Partial function generator to use for recover.
    * Note this is the AWSErrorDetails error code, not the SdkException HTTP status code.
    * If its an AWSServiceException with the given code return details.
    * Else Option.empty
    */
  def awsSdkErrorCode(code: String)(e: Throwable): Option[AwsErrorDetails] = {
    awsErrorDetails(e).flatMap { awsInfo =>
      if (awsInfo.errorCode.equals(code)) awsInfo.some
      else None
    }
  }

  /** Generates a Partial Function to use in recover clauses that matches the code
    * to AWS EC2 Exceptions AwsErrorDetails, returning to if matches .
    * Add example code. recover gone away?
    */
  def recoverEc2ErrorCode[T](code: String, to: T): PartialFunction[Throwable, T] = {
    def fn(e: Throwable) = awsErrorDetails(e)
      .flatMap(details => if (details.errorCode == code) to.some else None)

    Function.unlift(fn)

  }

  def catchNestedAwsError[F[_], A, T <: Throwable: ClassTag](
      io: F[A]
  )(catcher: PartialFunction[Throwable, F[A]])(implicit ME: MonadError[F, Throwable]) = {
    io.handleErrorWith { err =>
      val handled: Option[F[A]] = if (err.isInstanceOf[java.util.concurrent.CompletionException]) {
        catcher.lift(err.getCause)
      } else Option.empty[F[A]]

      handled.getOrElse(ME.raiseError(err))
    }
  }

  /** IO.delay(fn).handleErrorWith(awsNestedErrorHandler { case foo:SomeEx if bool => IO.raiseError or IO.pure(fo)
    * Like a partial redeem
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

/** My AWS Domain Error Class */
case class OAWSErr(msg: String) extends Throwable
