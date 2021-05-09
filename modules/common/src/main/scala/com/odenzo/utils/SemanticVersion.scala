package com.odenzo.utils
import cats.syntax.all._

import scala.util.matching.Regex

/** Hack class for x.y.z style semantic versioning */
case class SemanticVersion(mega: Int, major: Int, minor: Int) {}

object SemanticVersion {
  val pattern: Regex = """(\d+).(\d+).(\d+)""".r

  implicit val ordering: Ordering[SemanticVersion] = Ordering.by(unapply)

  def fromParts(mega: Int, major: Int, minor: Int): String = s"$mega.$major.$minor"

  // Overflow errors not caught, be sane.
  def fromString(s: String): Option[SemanticVersion] =
    s match {
      case pattern(mg, mj, mn) => SemanticVersion(mg.toInt, mj.toInt, mn.toInt).some
      case _                   => None
    }
}
