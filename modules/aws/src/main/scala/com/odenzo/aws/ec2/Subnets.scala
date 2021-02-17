package com.odenzo.aws.ec2

import cats._
import cats.data._
import cats.effect.{ContextShift, IO}
import cats.syntax.all._
import com.odenzo.aws.{CIDR, OTags}
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, OError}
import fs2.Stream
import software.amazon.awssdk.services.ec2.model._

import scala.jdk.CollectionConverters._

/** This needs a cleanup going to one describeSubnet w/ Filters
  * [https://sdk.amazonaws.com/java/api/latest/software/amazon/awssdk/services/ec2/model/DescribeSubnetsRequest.Builder.html]
  */
object Subnets {

  /** Create a subnet, without routing rules or Nat/IGW, returning subnet on success.
    * This call also doesn't assign a public IPv4 addresss, which is needed
    */
  def createSubnet(vpc: Vpc, az: String, cidr: CidrBlock, otags: OTags)(implicit cs: ContextShift[IO]): IO[Subnet] = {
    IOU
      .toIO(
        EC2.client.createSubnet(
          CreateSubnetRequest.builder
            .vpcId(vpc.vpcId)
            .availabilityZone(az)
            .cidrBlock(cidr.cidrBlock)
            .tagSpecifications(TagSpecification.builder().resourceType(ResourceType.SUBNET).tags(otags.via(EC2.toEC2Tag)).build())
            .build()
        )
      )
      .map(_.subnet)
  }

  /** Mastwr Call  For a list of filter types see AWS Javadocs */
  def describeSubnets(filters: Filter*)(implicit cs: ContextShift[IO]): IO[Stream[IO, Subnet]] = {
    val withFilters = DescribeSubnetsRequest.builder.filters(filters.asJavaCollection).build()
    for {
      stream <- FS2Utils.toStream(EC2.client.describeSubnetsPaginator(withFilters))
      content = stream.map(_.subnets().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  /** Master Call */
  def listSubnets(filters: Filter*)(implicit cs: ContextShift[IO]): IO[List[Subnet]] =
    describeSubnets(filters: _*).flatMap(_.compile.toList).redeemWith(e => IO.raiseError(OError("list Subnet Streaming Error", e)), IO.pure)

  def listSubnets(inVpc: Vpc)(implicit cs: ContextShift[IO]): IO[List[Subnet]] =
    listSubnets(EC2.filter("vpc-id", inVpc.vpcId()))

  /** Finds first subnet in VPC with CIDR. SHould be 0 or 1 always. Error on 0 */
  def findSubnetByCidr(vpc: Vpc, cidr: CIDR)(implicit cs: ContextShift[IO]): IO[Option[Subnet]] =
    listSubnets(EC2.vpcFilter(vpc), EC2.filter("cidr-block", cidr.toString)) >>=
      IOU.optionOne(s"Subnet w/ CIDR $cidr in ${vpc.vpcId()}")

  def getSubnetByCidr(vpc: Vpc, cidr: String)(implicit cs: ContextShift[IO]): IO[Subnet] =
    CIDR.fromString(cidr).flatMap(findSubnetByCidr(vpc, _)) >>= IOU.required(s"Subnet $cidr")

  def findSubnetsWithTag(vpc: Vpc, tag: (String, String))(implicit cs: ContextShift[IO]): IO[List[Subnet]] =
    listSubnets(EC2.vpcFilter(vpc), EC2.tagFilter(tag._1, tag._2))

  /** Find subnets in VPC with Tag Name=`name`. */
  def findSubnetWithName(vpc: Vpc, name: String)(implicit cs: ContextShift[IO]): IO[Option[Subnet]] = {

    (listSubnets(EC2.vpcFilter(vpc), EC2.tagFilter("Name", name)) >>= IOU.optionOne(s"Subnets named $name"))
      .flatTap { sn =>
        IO(scribe.info(s"Subnet $name  was found? ${sn.isDefined} State: ${sn.map(_.state())}"))
      }
  }

  def getSubnetWithName(vpc: Vpc, name: String)(implicit cs: ContextShift[IO]): IO[Subnet] =
    findSubnetWithName(vpc, name) >>= IOU.required(s"subnet named $name not found")

  def findSubnetById(id: String)(implicit cs: ContextShift[IO]): IO[Option[Subnet]] =
    listSubnets(EC2.filter("subnet-id", id)) >>= IOU.optionOne(s"Subnet ID $id")

  def findSubnetBySubnetId(vpc: Vpc, id: String)(implicit cs: ContextShift[IO]): IO[Option[Subnet]] =
    listSubnets(EC2.vpcFilter(vpc), EC2.filter("subnet-id", id)) >>= IOU.optionOne(s"Duplicate Subnets $id")

  /** Adds/replaces given tags on the subnet */
  def tagSubnet(vpc: Vpc, cidr: CIDR, tags: OTags)(implicit cs: ContextShift[IO]): IO[Boolean] = {
    for {
      subnet <- findSubnetByCidr(vpc, cidr) >>= IOU.required(s"CIDR $cidr Subnet Not Found")
      rs     <- EC2.tagResource(subnet.subnetId, tags)
    } yield rs.sdkHttpResponse.isSuccessful
  }

  /** Turns on/off the autoAssignIPv4 - nothing of interest returned */
  def modifySubnetPublicIPv4(subnet: Subnet, autoAssignIPv4: Boolean)(implicit cs: ContextShift[IO]): IO[String] = {
    IOU
      .toIO(
        EC2.client.modifySubnetAttribute(
          ModifySubnetAttributeRequest.builder
            .subnetId(subnet.subnetId)
            .mapPublicIpOnLaunch(SecurityGroupHelpers.buildBoolean(autoAssignIPv4))
            .build()
        )
      )
      .map(_.responseMetadata().requestId())
  }

  // Not sure if we will have to wait here
  def deleteSubnet(sn: Subnet)(implicit cs: ContextShift[IO]): IO[Unit] =
    IO(scribe.debug(s"Deleting Subnet ${oprint(sn)}")) *>
      IOU.toIO(EC2.client.deleteSubnet(DeleteSubnetRequest.builder.subnetId(sn.subnetId).build())).void
}
