package com.odenzo.aws.eks

import cats._
import cats.data._
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._
import com.odenzo.aws.AwsErrorUtils
import com.odenzo.aws.iam.IAM
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, RetryableError}
import fs2._
import software.amazon.awssdk.services.eks.EksAsyncClient
import software.amazon.awssdk.services.eks.model._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
object EKS {

  /** Autoclosing Session Client MT Safe */
  final lazy val client: EksAsyncClient = EksAsyncClient.create()

  def listClusters()(implicit cs: ContextShift[IO]): IO[Stream[IO, String]] = {
    for {
      stream <- FS2Utils.toStream(client.listClustersPaginator())
      burst   = stream.map(_.clusters().asScala.toList).flatMap(l => Stream.emits(l))
    } yield burst
  }

  /** See if a cluster exists */
  def clusterExists(name: String)(implicit cs: ContextShift[IO]): IO[Boolean] = {
    listClusters().flatMap(stream => stream.filter(_.equals(name)).compile.last).map(_.isDefined)
  }

  def findCluster(name: String)(implicit cs: ContextShift[IO]): IO[Option[Cluster]] = {
    describeCluster(name).redeem(AwsErrorUtils.nestedRecoverToOption[ResourceNotFoundException], cluster => cluster.some)
  }

  def describeCluster(name: String)(implicit cs: ContextShift[IO]): IO[Cluster] = {
    IOU
      .toIO(client.describeCluster(DescribeClusterRequest.builder.name(name).build()))
      .map(_.cluster())
  }

  def deleteCluster(name: String)(implicit cs: ContextShift[IO]): IO[Cluster] = {
    IOU
      .toIO(client.deleteCluster(DeleteClusterRequest.builder().name(name).build()))
      .map(_.cluster)
  }

  def findNodegroup(clusterName: String, nodegroupName: String)(implicit cs: ContextShift[IO]): IO[Option[Nodegroup]] = {
    describeNodegroup(clusterName, nodegroupName)
      .redeem(AwsErrorUtils.nestedRecoverToOption[ResourceNotFoundException], ng => ng.some)
  }

  def deleteNodegroup(clusterName: String, ngName: String)(implicit cs: ContextShift[IO]): IO[Nodegroup] = {
    IOU
      .toIO(client.deleteNodegroup(DeleteNodegroupRequest.builder.clusterName(clusterName).nodegroupName(ngName).build()))
      .map(_.nodegroup)
  }

  /** All nodegroups in the cluster by name */
  def listNodegroups(clusterName: String)(implicit cs: ContextShift[IO]): IO[List[String]] = {
    for {
      str    <- FS2Utils.toStream(client.listNodegroupsPaginator(ListNodegroupsRequest.builder.clusterName(clusterName).build()))
      content = str.map(_.nodegroups().asScala.toList) >>= Stream.emits
      res    <- content.compile.toList
    } yield res

  }

  def describeNodegroup(clusterName: String, nodegroupName: String)(implicit cs: ContextShift[IO]): IO[Nodegroup] = {
    IOU
      .toIO(client.describeNodegroup(DescribeNodegroupRequest.builder.clusterName(clusterName).nodegroupName(nodegroupName).build()))
      .map(_.nodegroup())
  }

  /** For use with FS2 retry */
  def isNodegroupReady(ng: Nodegroup)(implicit cs: ContextShift[IO]): IO[Nodegroup] = {
    describeNodegroup(ng.clusterName(), ng.nodegroupName()).flatMap { rs =>
      val msg     = s"Nodegroup ${rs.nodegroupName} ${rs.status} - ${oprint(rs.health().issues())}"
      scribe.info(s"Checking $msg")
      def pending = IO.raiseError(RetryableError(msg))
      def failure = IO.raiseError(new IllegalStateException(msg))
      rs.status match {
        case NodegroupStatus.ACTIVE   => IO.pure(rs)
        case NodegroupStatus.CREATING => pending
        case NodegroupStatus.DELETING => pending
        case NodegroupStatus.UPDATING => pending
        case _                        => failure // DEGRADED treated as a failure too fpr waiting, perhaps "SUCCESS" though

      }
    }
  }

  def waitForNodegroupActive(ng: Nodegroup)(implicit cs: ContextShift[IO], t: Timer[IO]): IO[Nodegroup] = {
    FS2Utils.uniformRetry(30 seconds, 40)(isNodegroupReady(ng))
  }

  /**
    * THis waits for a cluster to go from UPGRADING to ACTIVE
    */
  def isClusterReady(clusterName: String)(implicit cs: ContextShift[IO]): IO[Cluster] = {
    for {
      cluster <- describeCluster(clusterName)
      msg      = s"EKS Cluster  $clusterName State  ${cluster.status()}"
      _        = scribe.info(msg)
      rs      <- cluster.status() match {
                   case ClusterStatus.ACTIVE                 => IO.pure(cluster)
                   case ClusterStatus.FAILED                 => IO.raiseError(new IllegalStateException(msg))
                   case ClusterStatus.UNKNOWN_TO_SDK_VERSION => IO.raiseError(new IllegalStateException(msg))
                   case _                                    => IO.raiseError(RetryableError(msg))
                 }
    } yield rs

  }

  /** Creates a cluster for the requests via API method */
  def executeCreateCluster(rq: CreateClusterRequest)(implicit cs: ContextShift[IO]): IO[Cluster] = {
    IOU.toIO(client.createCluster(rq)).map(_.cluster)
  }

  def waitForClusterAvailable(clusterName: String)(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[Cluster] = {
    // Cluster w/o Worker nodes about 10-20 minutes?
    FS2Utils.uniformRetry(30 seconds, 20)(isClusterReady(clusterName))
  }

  /** Attaches EKSClusterPolicy and some inline policies */
  def attachStandardEksServiceRolePolicies(roleName: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      _ <- IAM.attachRolePolicy(roleName, "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy")
      _ <- IAM.attachRoleInlinePolicy(roleName, "NLB", ClusterServiceRole.nlb)
      _ <- IAM.attachRoleInlinePolicy(roleName, "Cloudwatch", ClusterServiceRole.cloudWatchMetrics)
    } yield ()
  }

  /** Attaches EKSClusterPolicy and some inline policies */
  def attachStandardEksWorkerNodeRolePolicies(roleName: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      awsPolicies <- NodeInstanceRole.awsPolicies.traverse(IAM.getNamedPolicy)
      _           <- awsPolicies.traverse(p => IAM.attachRolePolicy(roleName, p.arn))
      _           <- IAM.attachRoleInlinePolicy(roleName, "AutoScaling", NodeInstanceRole.autoScaling)
      _           <- IAM.attachRoleInlinePolicy(roleName, "DNSChange", NodeInstanceRole.externalDNSChangeSet)
      _           <- IAM.attachRoleInlinePolicy(roleName, "DNSHostedZones", NodeInstanceRole.externalDNSHostedZones)
      // TODO: Combine S3 and other Managed Services Permissions and add
    } yield ()
  }

}
