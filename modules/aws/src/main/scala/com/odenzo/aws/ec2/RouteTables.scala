package com.odenzo.aws.ec2

import cats.effect._
import cats.syntax.all._
import com.odenzo.aws.ec2.EC2Errors.unknownError
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, RetryableError}
import fs2._
import software.amazon.awssdk.services.ec2.model._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.language.postfixOps

object RouteTables {

  /** Returns all route tables as a stream and filter that instead of passing in AWS filter. Not so many route tables
    * See: https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeRouteTables.html for Filter Info
    */
  def describeRouteTables(filters: Filter*): IO[Stream[IO, RouteTable]] = {
    val rq = DescribeRouteTablesRequest.builder.filters(filters.asJavaCollection).build()
    for {
      stream <- FS2Utils.toStream(EC2.client.describeRouteTablesPaginator(rq))
      content = stream.map(_.routeTables().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  def listRouteTables(filters: Filter*): IO[List[RouteTable]] = {
    describeRouteTables(filters: _*).flatMap(_.compile.toList)
  }

  /** BROKEN: Seems it can't deal wioth multiple tages so get one by one. */
  def findRouteTables(names: List[String]): IO[List[RouteTable]] = {
    val filters: List[Filter] = names.map(EC2.tagFilter("Name", _))
    scribe.info(s"RouteTable Name Tags: ${oprint(filters)}")
    // Might as well FS2 it with some parallelism
    filters.traverse(tag => listRouteTables(tag) >>= IOU.optionOne(s"$tag")).map(_.flatten)
  }

  def findRouteTable(rtId: String): IO[Option[RouteTable]] = {
    describeRouteTables(EC2.filter("route-table-id", rtId)).flatMap(_.compile.last)

  }

  /** A Route table is created in Vpc and associated to Vpc or subnets */
  def createRouteTable(vpc: Vpc): IO[RouteTable] = {
    IOU
      .toIO(EC2.client.createRouteTable(CreateRouteTableRequest.builder.vpcId(vpc.vpcId).build()))
      .map(_.routeTable())
  }

  /** Will delete all the RT associations */
  def disassociateRouteTable(rt: RouteTable): IO[List[DisassociateRouteTableResponse]] = {
    rt.associations().asScala.toList.map(_.routeTableAssociationId).traverse { id =>
      IOU.toIO(EC2.client.disassociateRouteTable(DisassociateRouteTableRequest.builder.associationId(id).build()))
    }
  }

  /** Delete the route table (for a subnet), may have to dissociate first */
  def deleteRouteTable(rt: RouteTable): IO[Unit] =
    IOU.toIO(EC2.client.deleteRouteTable(DeleteRouteTableRequest.builder.routeTableId(rt.routeTableId()).build())).void

  /** Few asssociation types, this only handles subnets. The association state may be PENDING, FAILED or Associated.
    * Currently that is not checked here. TODO: Consistency add refresh until not pending.
    */
  def associateRouteTableToSubnet(subnet: Subnet, rt: RouteTable): IO[AssociateRouteTableResponse] = {
    IOU.toIO(
      EC2.client.associateRouteTable(
        AssociateRouteTableRequest.builder
          .routeTableId(rt.routeTableId)
          .subnetId(subnet.subnetId)
          .build()
      )
    )
  }

  def findAssociationState(assocId: String, rt: RouteTable): IO[Option[RouteTableAssociationState]] = {
    for {
      rt   <- findRouteTable(rt.routeTableId()) >>= IOU.required(s"RouteTable ${rt.routeTableId}")
      state = rt.associations().asScala.find(_.routeTableAssociationId.equals(assocId)).map(_.associationState())
    } yield state
  }

  def retryAssocationState(assocId: String, rt: RouteTable): IO[String] = {
    for {
      status <- findAssociationState(assocId, rt) >>= IOU.required(s"Association Not Found $assocId")
      msg     = s"Association Id $assocId on RouteTable ${rt.routeTableId} Status ${status.state}"
      state   = status.state match {
                  case RouteTableAssociationStateCode.ASSOCIATING            => throw RetryableError(msg)
                  case RouteTableAssociationStateCode.ASSOCIATED             => msg
                  case RouteTableAssociationStateCode.DISASSOCIATING         => throw RetryableError(msg)
                  case RouteTableAssociationStateCode.DISASSOCIATED          => msg
                  case RouteTableAssociationStateCode.FAILED                 => throw new IllegalStateException(msg)
                  case RouteTableAssociationStateCode.UNKNOWN_TO_SDK_VERSION => throw new IllegalStateException(msg)
                }

    } yield state
  }

  def waitForAssociationState(assocId: String, rt: RouteTable): IO[String] = {
    scribe.info(s"Waiting for Association $assocId of ${rt.routeTableId}")
    FS2Utils.uniformRetry(10 seconds, 30)(retryAssocationState(assocId, rt))
  }

  /** Bit of a hack because freshly created VPC has just one default route table */
  def routeTablesForVpc(vpc: Vpc): IO[Stream[IO, RouteTable]] = {
    describeRouteTables(EC2.filter("vpc-id", vpc.vpcId))
  }

  /** Bit of a hack because freshly created VPC has just one default route table
    * I want to find the main route tables, shows up in console,not tags.
    */
  def getMainRouteTableForVpc(vpc: Vpc): IO[RouteTable] = {
    describeRouteTables(EC2.filter("association.main", "true"), EC2.vpcFilter(vpc)).flatMap(_.compile.lastOrError)
  }

  def findRouteTableForSubnet(subnet: Subnet): IO[Option[RouteTable]] = {
    describeRouteTables(EC2.filter("association.subnet-id", subnet.subnetId)).flatMap(_.compile.last)
  }

  /** Quickie to add a route to RouteTable for internet gateway. Typically cidr 0.0.0.0/0 */
  def addRoute(rt: RouteTable, igw: InternetGateway, cidr: CidrBlock): IO[Unit] = {
    IOU
      .toIO(
        EC2.client.createRoute(
          CreateRouteRequest.builder
            .destinationCidrBlock(cidr.cidrBlock)
            .gatewayId(igw.internetGatewayId)
            .routeTableId(rt.routeTableId)
            .build()
        )
      )
      .ensure(unknownError("Adding Route to VPC"))(_.returnValue)
      .void
  }

  def addRoute(rt: RouteTable, nat: NatGateway, cidr: CidrBlock): IO[Unit] = {
    IOU
      .toIO(
        EC2.client.createRoute(
          CreateRouteRequest.builder
            .destinationCidrBlock(cidr.cidrBlock)
            .natGatewayId(nat.natGatewayId)
            .routeTableId(rt.routeTableId)
            .build()
        )
      )
      .ensure(unknownError("Adding Route to VPC"))(_.returnValue)
      .void
  }

  def addLocalRoute(rt: RouteTable, cidr: CidrBlock): IO[Unit] = {
    IOU
      .toIO(
        EC2.client.createRoute(
          CreateRouteRequest.builder
            .destinationCidrBlock(cidr.cidrBlock)
            .routeTableId(rt.routeTableId)
            .build()
        )
      )
      .ensure(unknownError("Adding Route to VPC"))(_.returnValue)
      .void
  }

  //def describeLocalGatewayRouteTableVpcAssociationsRequest
}
