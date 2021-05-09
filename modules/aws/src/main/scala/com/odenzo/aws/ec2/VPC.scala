package com.odenzo.aws.ec2

import cats.effect._
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, OTag, OTags}
import com.odenzo.utils.{FS2Utils, IOU, OError, RetryableError}
import fs2._
import software.amazon.awssdk.services.ec2.model._

import scala.jdk.CollectionConverters._

/** Networking Stuff -  VPC, ENI, EIP, IGW (Should move to NatGateways */
object VPC {

  def describeVPCs(): IO[Stream[IO, Vpc]] = {
    for {
      stream <- FS2Utils.toStream { EC2.client.describeVpcsPaginator }
      content = stream.map(_.vpcs().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  /** Finds the VPC with eksctl.cluster.k8s.io cluster name. Not terribly stable. EmptyList is not found.
    * Should always have 0 or 1 really.
    */
  def filterVpc(fn: Vpc => Boolean): IO[List[Vpc]] = {
    for {
      stream <- describeVPCs()
      vpcs   <- stream.filter(fn).compile.toList
    } yield vpcs
  }

  def listVpcByTag(otag: OTag): IO[List[Vpc]] = {
    val tag: Tag           = EC2.toEC2Tag(otag.name, otag.content)
    val fn: Vpc => Boolean = _.tags().contains(tag)
    filterVpc(fn)
  }

  /** Finds 0 or 1 VPC with given tag */
  def findVpcByTag(tag: OTag): IO[Option[Vpc]] = {
    listVpcByTag(tag) >>= IOU.optionOne(s"VPC for $tag")
  }

  def getVpcByTag(tag: OTag): IO[Vpc] = {
    listVpcByTag(tag) >>= IOU.exactlyOne(s"VPC for $tag")
  }

  /** Raises error if more than one  found */
  def findVpcById(id: String): IO[Option[Vpc]] = {
    val fn: Vpc => Boolean = id === _.vpcId
    filterVpc(fn) >>= IOU.optionOne(s"VPC ID $id")
  }

  def getVpcById(id: String): IO[Vpc] = findVpcById(id) >>= IOU.required(s"VPC ID $id")

  def findVpcByClusterName(name: String): IO[Option[Vpc]] = {
    val tag = OTag("horn/cluster", s"$name")
    listVpcByTag(tag) >>= IOU.optionOne(s"VPC for $tag")
  }

  def getVpcByClusterName(name: String): IO[Vpc] =
    findVpcByClusterName(name) >>= IOU.required(s"VPC for Name $name")

  /** Creates a basic VPC with given CIDR Block - this is a building component. subnets etc to be added
    * This will return a VPC with a state (probably pending) Unnamed/tagged Not sure we can tag before its out of Pending
    */
  def createVpc(cidrBlock: CidrBlock): IO[Vpc] = {
    IOU
      .toIO(
        EC2.client.createVpc(
          CreateVpcRequest.builder
            .cidrBlock(cidrBlock.cidrBlock())
            .amazonProvidedIpv6CidrBlock(false)
            .build()
        )
      )
      .map(_.vpc())
  }

  def deleteVpc(vpc: Vpc): IO[Unit] = {
    IOU
      .toIO(EC2.client.deleteVpc(DeleteVpcRequest.builder.vpcId(vpc.vpcId).build()))
      .void
  }

  /** Enables  DnsSupport for the given (existing) VPC. AWS can only set one attribute at a time ^_^
    * DNS resolution is enabled by default, so enable DNsHostname  I think DNS Supprt is ClassicLink
    */
  def enableDnsOnVpc(vpc: Vpc): IO[ModifyVpcAttributeResponse] = {
    IOU.toIO(
      EC2.client.modifyVpcAttribute(
        ModifyVpcAttributeRequest.builder
          .vpcId(vpc.vpcId())
          .enableDnsHostnames(AttributeBooleanValue.builder.value(true).build())
          .build()
      )
    )
  }

  /** Stream of all Elastic Network Interfaces (ENI) */
  def listEni(): IO[Stream[IO, NetworkInterface]] = {
    for {
      stream <- FS2Utils.toStream(EC2.client.describeNetworkInterfacesPaginator())
      content = stream.map(_.networkInterfaces().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  def listEni(inVpc: Vpc): IO[List[NetworkInterface]] = {
    listEni().flatMap { stream =>
      stream.filter(_.vpcId.equals(inVpc.vpcId)).compile.toList
    }
  }

  def deleteEni(id: String): IO[DeleteNetworkInterfaceResponse] = {
    IO(scribe.info(s"Deleteing ENI $id")) *>
      IOU.toIO(EC2.client.deleteNetworkInterface(DeleteNetworkInterfaceRequest.builder.networkInterfaceId(id).build()))

  }

  /** Creates an Internet Gateway and associated with given VPC */
  def createInternetGateway(vpc: Vpc): IO[InternetGateway] = {
    val assocIg    = AttachInternetGatewayRequest.builder.vpcId(vpc.vpcId).internetGatewayId(_: String).build
    val describeIg = DescribeInternetGatewaysRequest.builder.internetGatewayIds(_: String).build

    for {
      igwId <- IOU.toIO(EC2.client.createInternetGateway).map(_.internetGateway.internetGatewayId)
      _     <- IOU.toIO(EC2.client.attachInternetGateway(assocIg(igwId)))
      igw   <- IOU.toIO(EC2.client.describeInternetGateways(describeIg(igwId)))
      res   <- IO.fromOption(igw.internetGateways().asScala.headOption)(OError(s"Fetching Created IGW $igwId Failed"))
    } yield res
  }

  def listInternetGateways(vpc: Vpc): IO[List[InternetGateway]] = {
    IOU
      .toIO(
        EC2.client
          .describeInternetGateways(
            DescribeInternetGatewaysRequest.builder
              .filters(EC2.filter("attachment.vpc-id", vpc.vpcId))
              .build()
          )
      )
      .map(_.internetGateways.asScala.toList)

  }

  /** Creates EIP if doesn't exist, applies tags regardless */
  def createEipIffNeeded(tagName: String, tags: OTags): IO[Address] = {
    for {
      addrOpt <- VPC.findAddress(EC2.tagFilter("Name", tagName))
      addr    <- addrOpt match {
                   case Some(eip) => EC2.tagResource(eip.allocationId(), tags).as(eip)
                   case None      => allocateElasticIP(tags) *> VPC.getAddress(EC2.tagFilter("Name", tagName))
                 }
    } yield addr
  }

  /** Allocates an EIP for a VPC..but does not associate it. We should tag and name too
    * Tag using EC2.tagResource. To use we will assign to EC2 Instance.
    * See asssocateIpToInstance below
    */
  def allocateElasticIP(tags: OTags): IO[AllocateAddressResponse] = {
    IOU
      .toIO(EC2.client.allocateAddress(AllocateAddressRequest.builder.domain(DomainType.VPC).build()))
      .flatTap(aar => EC2.tagResource(aar.allocationId(), tags))
  }

  /** Find Elasteic IP by Name */
  def describeElasticIpNamed(named: String): IO[List[Address]] = {
    describeAddresses(List(EC2.tagFilter("Name", named)))
  }

  def describeElasticIP(tagKey: String, tagVal: String): IO[List[Address]] =
    describeAddresses(List(EC2.tagFilter(tagKey, tagVal)))

  def getAddress(filters: Filter*): IO[Address] =
    findAddress(filters: _*) >>= IOU.required(s"Adddress w/ Filters $filters")

  /** Find an Addresss/EIP ensuring 0 or 1 exist */
  def findAddress(filters: Filter*): IO[Option[Address]] = {
    describeAddresses(filters) >>= IOU.optionOne(s"EIP Filteres $filters")
  }

  def describeAddresses(filters: Seq[Filter]): IO[List[Address]] = {
    // No scrolling of pagination for this one.
    IOU
      .toIO(
        EC2.client.describeAddresses(
          DescribeAddressesRequest.builder
            .filters(filters.asJavaCollection)
            .build()
        )
      )
      .map(_.addresses().asScala.toList)
  }

  /** Associates an already allocated EIP with an ENI, for now the ENI on an EC2 node
    * Allows re-association and returns the association id.
    * If an instance has more than one ENI this cannot be used (as is common for kubernetes)
    */
  def associateElasticIP2Instance(eipAllocationId: String, ec2InstanceId: String): IO[String] = {
    IOU
      .toIO(
        EC2.client.associateAddress(
          AssociateAddressRequest.builder
            .allocationId(eipAllocationId)
            .allowReassociation(true)
            .instanceId(ec2InstanceId)
            //.networkInterfaceId(ec2InstanceId)
            .build()
        )
      )
      .map(_.associationId())
  }

  def associateElasticIP2Eni(eipAllocationId: String, eniId: String): IO[String] = {
    IOU
      .toIO(
        EC2.client.associateAddress(
          AssociateAddressRequest.builder
            .allocationId(eipAllocationId)
            .allowReassociation(true)
            .networkInterfaceId(eniId)
            .build()
        )
      )
      .map(_.associationId())
  }

  /** Unbind an EIP from its endpoint, idempotent */
  def deAssociateEIP(associationId: String): IO[Unit] = {
    IOU
      .toIO(
        EC2.client.disassociateAddress(
          DisassociateAddressRequest.builder
            .associationId(associationId)
            .build()
        )
      )
      .void
  }

  def describeAggregateIds(): IO[List[IdFormat]] = {
    IOU.toIO(EC2.client.describeAggregateIdFormat()).map(_.statuses().asScala.toList)
  }

  def describeNetworkInterfaceForAssociationId(associationId: String): IO[Option[NetworkInterface]] = {
    val f = EC2.filter("association.association-id", associationId)
    describeNetworkInterface(List(f)).flatMap(_.compile.toList) >>= IOU.optionOne(s"ENI for Association $associationId")
  }

  /** ENI describe */
  def describeNetworkInterface(filters: List[Filter]): IO[Stream[IO, NetworkInterface]] = {

    FS2Utils.toBurstStream(
      EC2.client.describeNetworkInterfacesPaginator(
        DescribeNetworkInterfacesRequest.builder
          .filters(filters.asJavaCollection)
          //.networkInterfaceIds()
          .build()
      )
    )((t: DescribeNetworkInterfacesResponse) => t.networkInterfaces().asScala)

  }

  /** This is only applicable to creating a vpc, not deleting it. */
  def checkVpcState(vpc: Vpc): IO[Vpc] = {
    getVpcById(vpc.vpcId).map { v =>
      val msg = s"VPC ${v.vpcId} State: ${v.state}"
      scribe.info(s"$msg")
      v.state match {
        case VpcState.AVAILABLE => v
        case VpcState.PENDING   => throw RetryableError(msg)
        case _                  => throw new IllegalStateException(msg)
      }
    }
  }

  /** Waits while VPC is PENDING until its available. If it hits a different state throws error.
    * If VPC can't be found throws error
    */
  def waitForVpcResolution(vpc: Vpc): IO[Vpc] = {
    def checkNotReady(vpc: Vpc) = {
      vpc.state match {
        case VpcState.AVAILABLE => false
        case VpcState.PENDING   => true
        case _                  => throw new IllegalStateException(s"VPC ${vpc.vpcId} in bad state ${vpc.state}")
      }
    }
    AWSUtils.pollWhile(checkNotReady)(getVpcById(vpc.vpcId))
  }

}
