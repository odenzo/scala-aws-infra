package com.odenzo.utils

import cats.syntax.all._

/** Utilities for dealing with environment variables and system properties */
object EnvProperties {

  def findProperty(name: String): Option[String] = scala.sys.props.get(name)

  def findEnvVar(name: String): Option[String] = scala.sys.env.get(name)

  /** Gets the property falling back to environment variable */
  def findPropOrEnv(name: String): Option[String] =
    findProperty(name) match {
      case Some(v) => v.some
      case None    => findEnvVar(name)
    }

}

/** Represent a (Unix) environment variable.
  */
case class EnvVar(name: String, value: String) {
  def toZsh: String = s"export $name=$value\n"
}

object EnvVar {

  /** Normalized the env var name */
  def normalize(k: String): String = k.toUpperCase.replace('-', '_')
}
