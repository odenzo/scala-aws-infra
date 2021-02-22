package com.odenzo.aws.ec2

import cats._
import cats.data._
import cats.effect.syntax.all._
import cats.effect.{ContextShift, IO}
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, AwsErrorUtils, OTag}
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, OError}
import software.amazon.awssdk.services.ec2.model._

import scala.jdk.CollectionConverters._

object SecurityGroups extends AwsErrorUtils with AWSUtils {

  /** Creates a security group if doesn't exist, then ensures the given inbound and outbound rules are there. Won't remove old rules,
    * better to delete security group to do that.
    */
  def constructSecurityGroupIfNeeded(name: String, desc: String, vpc: Vpc, inbound: List[IpPermission], outbound: List[IpPermission])(
      implicit cs: ContextShift[IO]
  ): IO[SecurityGroup] = {

    val inboundRules  = inbound.flatMap(in => in.ipRanges().asScala.map(_.description()))
    val securityGroup = findSecurityGroupByNameInVpc(name, vpc).flatMap {
      case Some(sg) => IO(scribe.warn(s"Using existing Security Group: ${sg.groupName}")) *> IO.pure(sg)
      case None     => IO(scribe.debug(s"Creating SG $name - $desc")) *> createSecurityGroup(name, desc, vpc)
    }

    for {
      sg   <- securityGroup
      _    <- IO(scribe.debug(s"Adding ${inbound.length} inbound rules: ${oprint(inboundRules)}"))
      _    <- addInboundRules(sg, inbound).unlessA(inbound.isEmpty)
      _    <- addOutboundRules(sg, outbound).unlessA(outbound.isEmpty)
      _    <- IO(scribe.debug(s"Done adding inbound and utbound rulles"))
      upSg <- getSecurityGroupById(sg.groupId())
    } yield upSg
  }

  /** Creates the security group returning the security group id on success. No rules are added */
  def createSecurityGroup(name: String, desc: String, vpc: Vpc)(implicit cs: ContextShift[IO]): IO[SecurityGroup] = {
    IO(scribe.info(s"Creating Security Group $name")) *>
      completableFutureToIO(
        EC2.client.createSecurityGroup(
          CreateSecurityGroupRequest.builder
            .groupName(name.trim)
            .vpcId(vpc.vpcId)
            .description(desc)
            .build()
        )
      ).flatMap(rs => getSecurityGroupById(rs.groupId))
  }

  def getOrCreateSecurityGroup(name: String, desc: String, vpc: Vpc)(implicit cs: ContextShift[IO]): IO[SecurityGroup] = {
    findSecurityGroupByNameInVpc(name, vpc).flatMap {
      case Some(sg) => IO(scribe.info(s"Using Existing Security Group ${oprint(sg)}")) *> IO.pure(sg)
      case None     => createSecurityGroup(name, desc, vpc)
    }
  }

  /** Master finder that others delegate too. */
  def listSecurityGroups(filters: Filter*)(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, SecurityGroup]] = {
    FS2Utils.toBurstStream(
      EC2.client.describeSecurityGroupsPaginator(DescribeSecurityGroupsRequest.builder.filters(filters.asJavaCollection).build())
    )(_.securityGroups.asScala)
  }

  def findSecurityGroupById(groupId: String)(implicit cs: ContextShift[IO]): IO[Option[SecurityGroup]] = {
    completableFutureToIO(EC2.client.describeSecurityGroups(DescribeSecurityGroupsRequest.builder.groupIds(groupId).build()))
      .map(r => fromJList(r.securityGroups))
      .flatMap(IOU.optionOne(s"SG ID $groupId"))
  }

  def getSecurityGroupById(groupId: String)(implicit cs: ContextShift[IO]): IO[SecurityGroup] = {
    findSecurityGroupById(groupId) >>= IOU.required(s"SG $groupId")
  }

  /** Lots of security groups so don't want to stream the whole lot. Need to capture EC2 exception */
  def findSecurityGroupByNameInVpc(groupName: String, inVpc: Vpc)(implicit cs: ContextShift[IO]): IO[Option[SecurityGroup]] = {
    listSecurityGroups(EC2.filter("group-name", groupName), EC2.filter("vpc-id", inVpc.vpcId))
      .flatMap(_.compile.toList)
      .flatMap(l => IOU.optionOne(s"SG $groupName in ${inVpc.vpcId}")(l))
  }

  def listSecurityGroupsWithTag(vpc: Vpc, tag: OTag)(implicit cs: ContextShift[IO]): IO[List[SecurityGroup]] = {
    listSecurityGroups(EC2.tagFilter(tag), EC2.vpcFilter(vpc)).flatMap(_.compile.toList)
  }

  def deleteSecurityGroupByNameIffExist(name: String, vpc: Vpc)(implicit cs: ContextShift[IO]): IO[Option[Unit]] = {
    SecurityGroups.findSecurityGroupByNameInVpc(name, vpc) >>= IOU.whenDefined { sg =>
      SecurityGroups.deleteSecurityGroup(sg)
    }
  }

  def deleteSecurityGroup(sg: SecurityGroup)(implicit cs: ContextShift[IO]): IO[Unit] = {
    IO(scribe.debug(s"Deleting Security Group ${sg.groupName}")) *>
      completableFutureToIO(EC2.client.deleteSecurityGroup(DeleteSecurityGroupRequest.builder.groupId(sg.groupId).build())).void
  }

  /** This will detach the given security groups from all EC2 Instances/Network Interfaces then delete the group(s) */
  def expungeSecurtyGroups(vpc: Vpc, sgs: List[SecurityGroup])(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      ec2nodes <- EC2.findRunningInstanceInVpc(vpc)
      _        <- ec2nodes.traverse { ec2 => SecurityGroups.removeSecurityGroupsFromInstance(sgs, ec2) }
      _        <- sgs.traverse { sg => SecurityGroups.deleteSecurityGroup(sg) }
    } yield ()
  }

  /**
    *  Note: This will add rules but not remove existing ones not in the ipPermissions list.
    *  TODO: Make sure this is master to add inbound, and check if a call needs to be made or rules already mach
    *  But should we refresh SG or assume its current?
    */
  def addInboundRules(sg: SecurityGroup, ipPermissions: List[IpPermission])(implicit cs: ContextShift[IO]): IO[SecurityGroup] = {
    completableFutureToIO(
      EC2.client.authorizeSecurityGroupIngress(
        AuthorizeSecurityGroupIngressRequest.builder.groupId(sg.groupId).ipPermissions(ipPermissions.asJavaCollection).build()
      )
    ).as(sg).recover(AwsErrorUtils.recoverEc2ErrorCode("InvalidPermission.Duplicate", sg))
  }

  /** Adds inbound rule for depending on source of another SecurityGroup with all ports */
  def addInboundRules(sg: SecurityGroup, srcSG: SecurityGroup)(implicit cs: ContextShift[IO]): IO[SecurityGroup] = {
    addInboundRules(sg, List(createSecurityGroupIPPermission(srcSG, -1, Some(-1), "-1")))
  }

  def createSecurityGroupIPPermission(
      other: SecurityGroup,
      fromPort: Int,
      toPort: Option[Int],
      protocal: String = "-1",
      desc: String = ""
  ): IpPermission = {
    val realToPort: Int = toPort.getOrElse(fromPort)
    IpPermission.builder
      .ipProtocol(protocal)
      .fromPort(fromPort)
      .toPort(realToPort)
      .userIdGroupPairs(UserIdGroupPair.builder.groupId(other.groupId).description(desc).build())
      .build()
  }

  /** On success returned the passed in Security Group (not updated)
    * TODO: If already exists redeem error
    */
  def addOutboundRules(sg: SecurityGroup, ipPermissions: List[IpPermission])(implicit cs: ContextShift[IO]): IO[SecurityGroup] = {
    IO(scribe.debug(s"Adding Outbound Rules to ${sg}")) *>
      completableFutureToIO {
        EC2.client.authorizeSecurityGroupEgress(
          AuthorizeSecurityGroupEgressRequest.builder.groupId(sg.groupId).ipPermissions(ipPermissions.asJavaCollection).build()
        )
      }.as(sg).recover(AwsErrorUtils.recoverEc2ErrorCode("InvalidPermission.Duplicate", sg))
  }

  /** Security Groups are bound to EC2 Instances (mostly by eksctl now), this unbinds WIP */
  def removeSecurityGroupsFromInstance(sgNames: List[SecurityGroup], instance: Instance)(implicit cs: ContextShift[IO]): IO[Unit] = {
    // If the Instance has multiple (network) interfaces then we have to specify the interface id instead WTF.
    // I guess if its in the list of security groups then delete from *each* interface id.
    // There are ones assigned on partiulcar NICs but I think this has the union of them all
    // Technically would should check if more that one NIC, delete on instance if not otherwise on NIC
    val existingSGIds: List[String]   = instance.securityGroups().asScala.toList.map(_.groupId)
    val existingSGNames: List[String] = instance.securityGroups().asScala.toList.map(_.groupName)
    val toRemoveIds: List[String]     = sgNames.map(_.groupId)
    val toRemoveNames                 = sgNames.map(_.groupName)

    val (existToRemove, toKeep) = existingSGIds.partition(existing => toRemoveIds.contains(existing)) // Small number

    /** Closes over existToRemove */
    def deleteFromNic(ni: InstanceNetworkInterface): IO[Unit] = {
      val onNic: List[GroupIdentifier] = ni.groups.asScala.toList
      val remainingOnNic               = onNic.filterNot(nsg => toRemoveIds.contains(nsg.groupId))
      for {
        _ <- IO(scribe.debug(s"NIC ${ni.networkInterfaceId()} has  ${oprint(onNic)}"))
        _ <- IO(scribe.debug(s"Removing ${oprint(toRemoveNames)} leaving ${oprint(remainingOnNic)}"))
        _ <- IO.raiseWhen(remainingOnNic.isEmpty)(OError(s"Can't Delete last SG ${oprint(onNic)} from $ni"))
        _ <- setSecurityGroupsForNic(NonEmptyList.fromListUnsafe(remainingOnNic.map(_.groupId)), ni.networkInterfaceId())
      } yield ()
    }

    val removeFromInstanceOrNics: IO[Unit] = for {
      _   <- IO(scribe.debug(s"Removing ${oprint(toRemoveNames)} from ${oprint(existingSGNames)}"))
      nics = instance.networkInterfaces.asScala.toList
      _   <- nics match {
               case many if nics.length >= 2 => many.traverse(v => deleteFromNic(v)).void
               case _    =>
                 IO.fromOption(NonEmptyList.fromList(toKeep))(OError("Can't Delete Instance SG to Zero"))
                   .flatMap(nel => setSecurityGroupsForInstance(nel, instance))
             }
    } yield ()

    removeFromInstanceOrNics.whenA(existToRemove.nonEmpty)

  }

  /** Sets the EC2 Instances list of security group to the grops gived (by id). I think won't work if multiple network interfaces */
  def setSecurityGroupsForInstance(sgIds: NonEmptyList[String], instance: Instance)(implicit cs: ContextShift[IO]): IO[Unit] =
    completableFutureToIO {
      EC2.client.modifyInstanceAttribute(
        ModifyInstanceAttributeRequest.builder
          .instanceId(instance.instanceId)
          .groups(sgIds.toList.asJavaCollection)
          .build()
      )
    }.void

  /** We add to all the "interfaces"/"network interfaces"/"ENI" (same things)
    * Should really check to see if more than one ENI, if just one use set instance else set NIC
    */
  def addSecurityGroupsToInstances(vpc: Vpc, sgs: NonEmptyList[SecurityGroup], instancesTagged: OTag)(
      implicit cs: ContextShift[IO]
  ): IO[Unit] = {
    val sgIds = sgs.map(_.groupId) // Now way to get GroupIdentifier schmuxks
    for {
      instances <- EC2.findRunningInstanceInVpc(vpc, EC2.tagFilter(instancesTagged))
      _         <- instances.traverse { instance =>
                     instance.networkInterfaces().asScala.toList.traverse { ni =>
                       scribe.debug(s"${instance.instanceId()} - ${oprint(ni)}")
                       val existingSG = ni.groups().asScala.toList.map(_.groupId)
                       setSecurityGroupsForNic(sgIds ++ existingSG, ni.networkInterfaceId) // Lets try setting on instance
                     }
                   }
    } yield ()
  }

  /** Removes security groups by positively setting thee securityGroups to keep. */
  private def setSecurityGroupsForNic(sgIds: NonEmptyList[String], nicId: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    completableFutureToIO {
      EC2.client.modifyNetworkInterfaceAttribute(
        ModifyNetworkInterfaceAttributeRequest.builder
          .networkInterfaceId(nicId)
          .groups(sgIds.toList.asJavaCollection)
          .build()
      )
    }.void
  }
}
