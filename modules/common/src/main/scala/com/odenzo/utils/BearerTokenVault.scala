package com.odenzo.utils

import cats.Show
import com.odenzo.utils.OPrint.oprint
import io.circe.Codec
import io.circe.generic.extras._
import io.circe.generic.extras.semiauto._

/** Map from key, e,g, app, to a BearerToken secret */
case class BearerTokenVault(tokens: Map[String, BearerToken]) {

  def tokenFor(key: String): Option[BearerToken] = tokens.get(key)

  override def toString: String = oprint(tokens)

  def defaultToken: BearerToken = BearerToken("FUBAR")
}

case class BearerToken(token: Secret) {

  /** Returns unprotected underlying secret, careful with com.odenzo.utils.logging etc. */
  def secret: String = token.secret
}

object BearerToken      {
  def apply(s: String): BearerToken = BearerToken(Secret(s))

  implicit val c: Configuration          = Configuration.default
  implicit val codec: Codec[BearerToken] = deriveUnwrappedCodec[BearerToken]
  implicit val show: Show[BearerToken]   = Show.fromToString

}
object BearerTokenVault {

  implicit val c: Configuration               = Configuration.default
  implicit val codec: Codec[BearerTokenVault] = deriveUnwrappedCodec
  implicit val show: Show[BearerTokenVault]   = Show.fromToString

  def empty: BearerTokenVault = BearerTokenVault(Map.empty[String, BearerToken])
}
