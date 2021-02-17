package com.odenzo.aws.ec2

import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import software.amazon.awssdk.services.ec2.model.Instance

/** AWS Model representing EC2 instance or any instance really, priv == VPC IP */
case class NodeAddr(publicIp: Option[String], publicDns: Option[String], privateIp: String, privateDns: String)

object NodeAddr {

  def fromInstance(inst: Instance): NodeAddr = {
    NodeAddr(Option(inst.publicIpAddress()), Option(inst.publicDnsName()), inst.privateIpAddress(), inst.privateDnsName())
  }

  implicit val config: Configuration           = Configuration.default.withSnakeCaseMemberNames
  implicit val codec: Codec.AsObject[NodeAddr] = deriveConfiguredCodec[NodeAddr]
}
