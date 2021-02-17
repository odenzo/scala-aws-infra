package com.odenzo.aws.eks

import cats._
import cats.data.{Kleisli, _}
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._
import com.odenzo.aws.{AwsPEM, OTags}
import com.odenzo.utils.{FS2Utils, IOU, RetryableError}
import software.amazon.awssdk.services.ec2.model.Subnet
import software.amazon.awssdk.services.eks.model._

import java.time.Instant
import java.util.UUID
import java.util.concurrent.CompletionException
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** Facilities to create NodeGroups - specifically targetted for EKS Worker Node Groups
  * NOTE: THIS IS DEPRECATED ONCE NEW WAY WORKS AND WILL BE MOVED TO ATTIC or at least down to AWS
  */
object NodeGroups {

  /** Returns a partial request to continue filling in.
    * subnets must be tagges with key /kubernetes.io/cluster/<clusterName>
    */
  def createNodeGroupBaseRq(
      nodeGroupName: String,
      clusterName: String,
      roleArn: String,
      subnets: List[Subnet],
      tags: OTags
  ): CreateNodegroupRequest.Builder = {
    val clientRqToken = UUID.randomUUID().toString
    CreateNodegroupRequest.builder
      .clusterName(clusterName)
      .labels(tags.modifyName(_.replace('/', '_')).tags.asJava) // Picky about labels but not tags
      .subnets(subnets.map(_.subnetId).asJava)
      .clientRequestToken(clientRqToken)
      .nodegroupName(nodeGroupName)
      .nodeRole(roleArn)
      // .releaseVersion() // Used latest
      .tags(tags.tags.asJava)                                   // Tags for NodeGroup Itself

  }

  /** Allow admin access to the worker nodes -- supply the PEM for the admin typically */
  def addRemoteAccess(rq: CreateNodegroupRequest.Builder, key: AwsPEM): CreateNodegroupRequest.Builder = {
    rq.remoteAccess(
      RemoteAccessConfig.builder
        .ec2SshKey(key.name)
        //.sourceSecurityGroups(remoteSgIds.asJava) // Allow from anywhere?
        .build()
    )
  }

  def addSizing(
      rq: CreateNodegroupRequest.Builder,
      diskSize: Int,
      instanceType: String,
      min: Int,
      max: Int,
      desired: Int
  ): CreateNodegroupRequest.Builder = {
    rq.diskSize(diskSize) // GB
      .instanceTypes(instanceType)
      .amiType(AMITypes.AL2_X86_64)
      .scalingConfig(NodegroupScalingConfig.builder.desiredSize(desired).minSize(min).maxSize(max).build())
  }

  def createNodeGroup(rq: CreateNodegroupRequest)(implicit cs: ContextShift[IO]) = {
    scribe.info(s"Creating NodeGroup With ${rq}")
    IOU
      .toIO {
        EKS.client.createNodegroup(rq)
      }
      .adaptErr {
        case e: CompletionException =>
          val cause = e.getCause
          scribe.warn(s"""NodeGroup Creation Error ${e.getLocalizedMessage}
                         | Details: ${cause}
                         |""".stripMargin)
          throw e
      }
  }

  /** Iniates the deletion of a NodeGroup -- returning the NodeGroup. It may take some time for it to be deleted. */
  def deleteNodeGroup(cluster: String, nodegroup: String)(implicit cs: ContextShift[IO]) = {
    IOU
      .toIO(EKS.client.deleteNodegroup(DeleteNodegroupRequest.builder.clusterName(cluster).nodegroupName(nodegroup).build()))
      .map(_.nodegroup())
  }

  def describeNodegroup(cluster: String, ng: String)(implicit cs: ContextShift[IO]) = {
    IOU
      .toIO(EKS.client.describeNodegroup(DescribeNodegroupRequest.builder.clusterName(cluster).nodegroupName(ng).build()))
      .map(_.nodegroup())
  }

  /** State testing just for creation / updating */
  def isCreated(clusterName: String, nodegroup: String)(implicit cs: ContextShift[IO]) = {
    for {
      ng <- describeNodegroup(clusterName, nodegroup)
      _   = scribe.info(s"EKS NodeGroupr $nodegroup State @ ${Instant.now} ${ng.status}")
      msg = s"NodeGroup State: ${ng.statusAsString()}"
      rs <- ng.status() match {
              case NodegroupStatus.ACTIVE                 => IO.pure(ng)
              case NodegroupStatus.DEGRADED               => IO.pure(ng)
              case NodegroupStatus.UPDATING               => IO.raiseError(RetryableError(msg))
              case NodegroupStatus.CREATING               => IO.raiseError(RetryableError(msg))
              case NodegroupStatus.DELETING               => IO.raiseError(RetryableError(msg))
              case NodegroupStatus.CREATE_FAILED          => IO.raiseError(new IllegalStateException(msg))
              case NodegroupStatus.DELETE_FAILED          => IO.raiseError(new IllegalStateException(msg))
              case NodegroupStatus.UNKNOWN_TO_SDK_VERSION => IO.raiseError(new IllegalStateException(msg))

            }
    } yield rs

  }

  def waitForNodegroupAvailable(cluster: String, nodegroup: String)(implicit cs: ContextShift[IO], timer: Timer[IO]) = {
    FS2Utils.uniformRetry(30 seconds, 40)(isCreated(cluster, nodegroup))
  }
}
