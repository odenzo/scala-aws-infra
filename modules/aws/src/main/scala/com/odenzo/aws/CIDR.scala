package com.odenzo.aws

import cats.effect.IO
import software.amazon.awssdk.services.ec2.model.CidrBlock

/** ip = 10.1.0.0   range = 16 gives 10.1.0.0/16
  */
case class CIDR(ip: String, range: Int) {
  val dots: Array[Int]  = ip.split('.').map(_.toInt)
  val level16           = s"${dots(0)}.${dots(1)}"
  override def toString = ip + '/' + range
  lazy val toCidrBlock  = CidrBlock.builder.cidrBlock(ip + '/' + range).build()

  def sub24(sub: Int): CIDR = CIDR(s"$level16.$sub.0", 24)
  def sub19(sub: Int): CIDR = CIDR(s"$level16.${sub * 32}.0", 19)

}

/** Start of a crude CIDR with validation and subnet utils as needed but just a terrible hack for now */
object CIDR {

  val ALL = CIDR.fromStringUnsafe("0.0.0.0/0")

  /** Expects 1.0.0.0/16  or 1.2.0.0/24 style CIDR */
  def fromString(cidr: String): IO[CIDR] = IO(fromStringUnsafe(cidr))

  def fromStringUnsafe(cidr: String): CIDR = {
    val parts: Array[String] = cidr.split('/').ensuring(_.sizeIs == 2)
    val range                = parts(1).toInt
    CIDR(parts.head, range)
  }

}
