package com.odenzo.utils

/** Cheery pick - should bring in all and typed Errors. */
object OErrors {

  // Don't worry about tailrec
  def deepMessages(t: Throwable, msg: List[String] = List.empty): List[String] = {
    Option(t.getCause) match {
      case None                => List(t.getMessage)
      case Some(dt: Throwable) => t.getMessage :: deepMessages(dt, msg)
    }
  }

  /** Formats multline indented for easy and inneficiant error com.odenzo.utils.logging */
  def deep(t: Throwable): String = {
    deepMessages(t).mkString("\n\t", "\n\t", "\n")
  }

  def wrappingError(s: String, t: Throwable) = new Throwable(s, t)

}

/** New Application or Library Throwables should extend this. Not sealed or anything
  * Can put sealed below (?)
  */
trait AppError extends Throwable

final case class NotFound(message: String) extends AppError

object OError {

  def apply(s: String): Throwable               = new Throwable(s)
  def apply(s: String, e: Throwable): Throwable = new Throwable(s, e)
}
