package com.odenzo.aws.ec2

import cats._
import cats.data._
import cats.syntax.all._
import software.amazon.awssdk.services.ec2.model._

/** Some helpers to build requests.. typically in context of the given VPC, look at combining with RequestHelpers */
class VPCHelpers(val vpc: Vpc) extends SecurityGroupHelpers {

  final val CIDR_VPC: CidrBlock = CidrBlock.builder().cidrBlock(vpc.cidrBlock()).build()

  def ipRangeVpc(desc: String = "All in VPC"): IpRange = buildIpRange(CIDR_VPC, desc)

  /** Actually this should make all protocals, TCP and UDP */
  def allInVpc(): IpPermission =
    IpPermission.builder
      .ipProtocol("-1")
      .ipRanges(buildIpRange(CIDR_VPC, "All VPC Internal"))
      .build()
}
