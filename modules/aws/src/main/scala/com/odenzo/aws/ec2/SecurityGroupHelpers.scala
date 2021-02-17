package com.odenzo.aws.ec2

import com.odenzo.aws.CIDR
import software.amazon.awssdk.services.ec2.model._

import java.util.UUID

/** CIDR and Security Group Helpers */
trait SecurityGroupHelpers {

  final val ANY_PORT = -1

  final val UDP  = 17
  final val TCP  = 6
  final val SCTP = 132

  val CIDR_ALL: CidrBlock = buildCidr("0.0.0.0/0")
  val ipRangeAll: IpRange = buildIpRange(CIDR_ALL, "Any")

  val allTcpPermissions: IpPermission = IpPermission.builder.ipProtocol("TCP").fromPort(-1).toPort(-1).ipRanges(buildIpRangeAll()).build()

  def buildCidr(s: String): CidrBlock                 = CidrBlock.builder().cidrBlock(s).build()
  def buildCidr(c: CIDR): CidrBlock                   = buildCidr(c.ip + '/' + c.range)
  def buildBoolean(b: Boolean): AttributeBooleanValue = AttributeBooleanValue.builder().value(b).build()

  /** Impure call to get a unique request token as needed for some AWS APIs for indempotency calls
    * Maybe an eval always for this is better
    */
  def requestTokenIMP: String = UUID.randomUUID().toString

  def buildIpRange(cidr: CidrBlock, desc: String): IpRange = IpRange.builder.cidrIp(cidr.cidrBlock).description(desc).build()

  def buildIpRangeAll(desc: String = "All"): IpRange = buildIpRange(CIDR_ALL, desc)

  def buildTcpPermission(port: Int, range: IpRange): IpPermission = {
    IpPermission.builder().fromPort(port).toPort(port).ipProtocol("TCP").ipRanges(range).build()
  }

  def buildTcpPermission(port: Int, cidr: CidrBlock, desc: String): IpPermission = {
    IpPermission.builder().fromPort(port).toPort(port).ipProtocol("TCP").ipRanges(buildIpRange(cidr, desc)).build()
  }

  /** UDP Permissions from Anywhere on the port range */
  def buildUdpPermission(start: Int, end: Int, desc: String): IpPermission = {
    IpPermission.builder().fromPort(start).toPort(end).ipProtocol("UDP").ipRanges(buildIpRangeAll(desc)).build()
  }

  def sshFromAnywhere(): IpPermission = buildTcpPermission(22, CIDR_ALL, "SSH From Anywhere")

}

object SecurityGroupHelpers extends SecurityGroupHelpers
