package com.odenzo.utils
import cats.effect.SyncIO

/** DNS Names, this is actually a DNS Subdomain now.
  * Used alot in K8S, and have regular expression that must match.
  * This has utilities to help and also to wrap.
  * Default constructor DOES NOT validate or mutate/normalize.
  * This iwas started to deal with verification of kubernetes vs dns vs tag names etc ficking etc on aws
  * Lost in the shuffle mostly.
  */
case class DnsName(names: String)

object DnsName {

  // '[a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*')

  def normalize(s: String): String = {
    s.toLowerCase.replace('_', '-')
  }
}

/** DNS-1123 valid wrapper, '[a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*') */
case class DNS1123(value: String)

/** Host can be IP or DNS name for now */
case class EndPoint(host: String, port: Int)

object EndPoint {
  def fromString(s: String): SyncIO[EndPoint] = {
    SyncIO.apply {
      val host = s.takeWhile(_ != ':')
      val port = s.drop(host.length + 1).toInt
      EndPoint(host, port)
    }
  }
}
