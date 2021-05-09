package com.odenzo.aws.ec2

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.{OTag, OTags}
import com.odenzo.utils.{FS2Utils, IOU, RetryableError}
import fs2.Stream
import software.amazon.awssdk.services.ec2.model.{
  AllocateAddressRequest,
  CreateNatGatewayRequest,
  DescribeNatGatewaysRequest,
  DomainType,
  NatGateway,
  NatGatewayState,
  ResourceType,
  Subnet,
  TagSpecification,
  _
}

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.language.postfixOps

object NatGateways {

  /** Create a NAT and an Elastic IP and associate them together with the given subnet */
  def createNatForSubnet(subnet: Subnet, eipAllocationId: String, tags: OTags): IO[NatGateway] = {

    val tagSpec = TagSpecification.builder.resourceType(ResourceType.NATGATEWAY).tags(tags.via(EC2.toEC2Tag)).build()
    AllocateAddressRequest.builder().domain(DomainType.VPC)

    IOU
      .toIO(
        EC2.client.createNatGateway(
          CreateNatGatewayRequest.builder
            .subnetId(subnet.subnetId)
            .allocationId(eipAllocationId) // EIP allocation id Required!
            .clientToken(SecurityGroupHelpers.requestTokenIMP)
            .tagSpecifications(tagSpec)
            .build()
        )
      )
      .map(_.natGateway())
  }

  /** List all the Nat Gateways satisfying filters */
  def listNatGateways(filters: Filter*): IO[List[NatGateway]] = {
    val prog = for {
      stream <- FS2Utils.toStream(
                  EC2.client.describeNatGatewaysPaginator(DescribeNatGatewaysRequest.builder.filter(filters.asJavaCollection).build())
                )
      content = stream.map(_.natGateways.asScala.toList)
      burst   = content >>= Stream.emits
    } yield burst
    prog.flatMap(_.compile.toList)
  }

  def describeNatGateway(natId: String): IO[NatGateway] =
    listNatGateways(EC2.filter("nat-gateway-id", natId)) >>= IOU.exactlyOne(s"NAT Gateway id $natId")

  /** Finds NAT Gateway that is attached state */
  def findNatGateway(withTag: OTag, inState: String = "available"): IO[Option[NatGateway]] =
    listNatGateways(EC2.tagFilter(withTag), EC2.filter("state", inState)) >>= IOU.optionOne(s"NAT Gateway $withTag")

  def findNatGateway(tagged: OTag, inVpc: Vpc): IO[Option[NatGateway]] =
    listNatGateways(EC2.vpcFilter(inVpc), EC2.tagFilter(tagged)) >>= IOU.optionOne(s"NAT Gateway Tagged $tagged in VPC ${inVpc.vpcId}")

  def delete(natId: String): IO[String] =
    IOU.toIO(EC2.client.deleteNatGateway(DeleteNatGatewayRequest.builder.natGatewayId(natId).build())).map(_.natGatewayId())

  /** NatState - throws error if not DELETED or AVAILABLE for use in Retry */
  def natState(natId: String): IO[NatGateway] = {
    describeNatGateway(natId).flatMap { n: NatGateway =>
      val msg = s"NatGatway $natId State ${n.state}"
      scribe.info(s"Checked $msg")
      n.state match {
        case NatGatewayState.PENDING                => IO.raiseError(RetryableError(msg))
        case NatGatewayState.DELETING               => IO.raiseError(RetryableError(msg))
        case NatGatewayState.FAILED                 => IO.raiseError(new Throwable(msg))
        case NatGatewayState.UNKNOWN_TO_SDK_VERSION => IO.raiseError(new Throwable(msg))
        case NatGatewayState.DELETED                => IO.pure(n)
        case NatGatewayState.AVAILABLE              => IO.pure(n)
      }
    }
  }

  /** FIXME: Re=Write Waits untils not PENDING or DELETING , ie DELETED or AVAILABLE */
  def waitForNatGateway(nat: NatGateway): IO[NatGateway] = {
    FS2Utils.uniformRetry(10 seconds, 20)(natState(nat.natGatewayId))
  }
}
