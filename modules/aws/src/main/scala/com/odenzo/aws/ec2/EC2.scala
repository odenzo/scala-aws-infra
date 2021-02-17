package com.odenzo.aws.ec2

import cats._
import cats.data._
import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._
import com.odenzo.aws.{AwsErrorUtils, OTag, OTags}
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, RetryableError}
import software.amazon.awssdk.services.ec2.Ec2AsyncClient
import software.amazon.awssdk.services.ec2.model._

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
object EC2 {

  import scala.jdk.CollectionConverters._

  val toEC2Tag: (String, String) => Tag = (k: String, v: String) => Tag.builder().key(k).value(v).build()

  def filter(key: String, values: Seq[String]): Filter = Filter.builder().name(key).values(values.asJavaCollection).build()
  def filter(key: String, value: String): Filter       = Filter.builder().name(key).values(value).build()
  def tagFilter(key: String, value: String): Filter    = Filter.builder().name(s"tag:$key").values(value).build()
  def tagFilter(t: OTag): Filter                       = tagFilter(t.name, t.content)
  def vpcFilter(vpc: Vpc): Filter                      = EC2.filter("vpc-id", vpc.vpcId())

  private[ec2] def tagSpecs(resourceType: ResourceType, tags: OTags): TagSpecification = {
    TagSpecification.builder().resourceType(resourceType).tags(tags.via(toEC2Tag)).build()
  }

  lazy val client: Ec2AsyncClient = Ec2AsyncClient.create()

  /** This may as well return IO[Unit] as nothing of interest in the response */
  def tagResources(resourceIds: List[String], tags: OTags)(implicit cs: ContextShift[IO]): IO[CreateTagsResponse] = {
    IOU.toIO(
      client.createTags(
        CreateTagsRequest.builder
          .resources(resourceIds.asJavaCollection)
          .tags(tags.via(toEC2Tag))
          .build()
      )
    )
  }

  def tagResource(resourceId: String, tags: OTags)(implicit cs: ContextShift[IO]): IO[CreateTagsResponse] = {
    IO(scribe.debug(s"Tagging Resource $resourceId w/ $tags")) *> tagResources(List(resourceId), tags)

  }

  def createKeyPair(keyname: String, tags: OTags)(implicit cs: ContextShift[IO]): IO[CreateKeyPairResponse] = {
    IOU
      .toIO(
        client.createKeyPair(
          CreateKeyPairRequest.builder
            .keyName(keyname)
            .tagSpecifications(tagSpecs(ResourceType.KEY_PAIR, tags))
            .build()
        )
      )
  }

  def listKeyPairs(filters: Filter*)(implicit cs: ContextShift[IO]): IO[List[KeyPairInfo]] = {
    IOU
      .toIO(client.describeKeyPairs(DescribeKeyPairsRequest.builder.filters(filters.asJavaCollection).build()))
      .map(_.keyPairs().asScala.toList)
  }

  /** Just to avoid dealing with AWS FU exceptions */
  def findKeyPairByName(keyname: String)(implicit cs: ContextShift[IO]): IO[Option[KeyPairInfo]] = {
    listKeyPairs(EC2.filter("key-name", keyname)) >>= IOU.optionOne(s"KeyPair $keyname")
  }

  def deleteKeyPairIfExists(name: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    findKeyPairByName(name).flatMap(opt => deleteKeyPair(name).unlessA(opt.isEmpty))
  }
  def deleteKeyPair(name: String)(implicit cs: ContextShift[IO]): IO[String] = {
    IOU
      .toIO(EC2.client.deleteKeyPair(DeleteKeyPairRequest.builder.keyName(name).build()))
      .as(s"Deleted KeyPair $name")
      .recover(AwsErrorUtils.recoverEc2ErrorCode("InvalidPermission.Duplicate", s"KeyPair $name not found.")) // Test and see the error
  }

  /** Create an EC2 given the AMI and the instance type. Wonder if we should/have to create a launchTemplate */
  def createAndRunInstance(
      keyName: String,
      subnet: Subnet,
      amiId: String,
      instanceType: String,
      securityGroups: NonEmptyList[SecurityGroup],
      tags: OTags
  )(implicit cs: ContextShift[IO]): IO[Instance] = {
    val clientToken = UUID.randomUUID().toString

    val rq = RunInstancesRequest.builder
      .clientToken(clientToken)
      .imageId(amiId)
      .instanceType(instanceType)
      .keyName(keyName)
      .minCount(1)
      .maxCount(1)
      //  .hibernationOptions(HibernationOptionsRequest.builder.configured(true).build())
      .instanceInitiatedShutdownBehavior(ShutdownBehavior.STOP)
      .monitoring(RunInstancesMonitoringEnabled.builder.enabled(true).build())
      .subnetId(subnet.subnetId())
      .securityGroupIds(securityGroups.map(_.groupId()).toList.asJavaCollection)
      //.blockDeviceMappings() // Can use this to make an boot disk storage?
      .tagSpecifications(tagSpecs(ResourceType.INSTANCE, tags))
      .build()

    scribe.info(s"EC2 RunInstance REquest: ${oprint(rq)}")
    IOU.toIO(client.runInstances(rq)).map(_.instances.asScala.toList) >>= IOU.exactlyOne(s"EC2 Instance")
  }

  def describeInstanceStatus(id: String)(implicit cs: ContextShift[IO]): IO[InstanceStatus] = {
    IOU
      .toIO(
        client.describeInstanceStatus(
          DescribeInstanceStatusRequest.builder
            .instanceIds(id)
            .includeAllInstances(true)
            .build()
        )
      )
      .map(_.instanceStatuses().asScala.toList)
      .flatMap(IOU.exactlyOne(s"No Status for EC2 Instance $id"))
  }

  /** TODO FIXME: This is a bit of a pain... has state which goes to running, then status is initializing. Check Both? */
  def waitForInstanceReady(id: String)(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[InstanceStatus] = {
    // TODO. switch to custom Cats Retry style with shouldRetrtFn
    FS2Utils.uniformRetry(10 seconds, 20) {
      describeInstanceStatus(id).flatMap { status: InstanceStatus =>
        scribe.info(s"Checked EC2 Instance  $id  ${Instant.now}  => ${status.instanceState.name} ${status.instanceStatus.status}")
        status.instanceState.name match {
          case InstanceStateName.PENDING => IO.raiseError(RetryableError("PENDING"))
          case InstanceStateName.RUNNING => IO.pure(status)
          case other                     => IO.raiseError(new IllegalMonitorStateException(s"Instance $id in state $other"))
        }
      }
    }
  }

  def listAwsAmi()(implicit cs: ContextShift[IO]): IO[List[Image]] = {
    IOU.toIO(client.describeImages(DescribeImagesRequest.builder.owners("amazon").build())).map(_.images.asScala.toList)
  }

  def findInstanceInVpcByName(vpc: Vpc, instanceName: String)(implicit cs: ContextShift[IO]): IO[Option[Instance]] = {
    findRunningInstanceInVpc(vpc, tagFilter("Name", instanceName)) >>= IOU.optionOne(s"EC2 $instanceName")
  }

  /** Finds running EC2 instances with optional filters, at most one tagFilter though! */
  def findRunningInstanceInVpc(vpc: Vpc, xtraFilters: Filter*)(implicit cs: ContextShift[IO]): IO[List[Instance]] = {
    val filters = Seq(filter("vpc-id", vpc.vpcId), filter("instance-state-name", "running")) ++ xtraFilters
    describeInstances(filters: _*)
      .flatMap(_.compile.toList)
      .flatTap { l: List[Instance] =>
        val iString = l.map(instanceToNiceString)
        IO(scribe.debug(s"EC2 Instances for ${vpc.cidrBlock} Filters: $xtraFilters: ${oprint(iString)}"))
      }
  }

  def instanceToNiceString(i: Instance): String = {
    val iName = i.tags.asScala.toList
      .filter(_.key().equals("Name"))
      .map(tag => s"Name: ${tag.value}")
    s"""${i.instanceId} - ${iName} - # Networks: ${i.networkInterfaces().asScala.toList.length}"""
  }

  def getInstanceById(id: String)(implicit cs: ContextShift[IO]): IO[Instance] =
    describeInstances(EC2.filter("instance-id", id)).flatMap(_.compile.last) >>= IOU.required(s"EC2 Instance Id $id")

  def findIpAddressesForInstance(vpc: Vpc, name: String)(implicit cs: ContextShift[IO]): IO[NodeAddr] = {
    for {
      inst <- findInstanceInVpcByName(vpc, name) >>= IOU.required(s"Running EC2 Named $name")
    } yield NodeAddr(Option(inst.publicIpAddress()), Option(inst.publicDnsName()), inst.privateIpAddress(), inst.privateDnsName())
  }

  def listInstances(filters: Filter*)(implicit cs: ContextShift[IO]): IO[List[Instance]] = {
    describeInstances(filters: _*).flatMap(_.compile.toList)
  }

  /** This expands out the Node Instances from each Reservation */
  def describeInstances(filters: Filter*)(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, Instance]] = {
    val rq = DescribeInstancesRequest.builder.filters(filters.asJavaCollection).build()
    for {
      stream <- FS2Utils.toStream(client.describeInstancesPaginator(rq))
      content = stream.map(rs => rs.reservations.asScala.toList.flatMap(reserv => reserv.instances().asScala.toList))
      burst   = content.flatMap(fs2.Stream.emits)
    } yield burst

  }

}
