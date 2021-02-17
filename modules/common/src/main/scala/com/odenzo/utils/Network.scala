package com.odenzo.utils

import cats.effect.IO

/** DNS-1123 valid wrapper, '[a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*') */
case class DNS1123(value: String)

/** Host can be IP or DNS name for now */
case class EndPoint(host: String, port: Int)

object EndPoint {
  def fromString(s: String) = {
    IO {
      val host = s.takeWhile(_ != ':')
      val port = s.drop(host.length + 1).toInt
      EndPoint(host, port)
    }
  }

}
