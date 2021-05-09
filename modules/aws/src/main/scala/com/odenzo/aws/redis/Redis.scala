package com.odenzo.aws.redis

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.rds.RDS.findCluster
import com.odenzo.aws.{AWSUtils, AwsErrorUtils, OTags}
import com.odenzo.utils.{FS2Utils, IOU}
import fs2._
import software.amazon.awssdk.services.ec2.model.SecurityGroup
import software.amazon.awssdk.services.elasticache.ElastiCacheAsyncClient
import software.amazon.awssdk.services.elasticache.model._
import software.amazon.awssdk.services.rds.model.DBCluster

import scala.jdk.CollectionConverters._

case class RedisConfig(
    name: String,
    nodeType: String,
    multiAZ: Boolean,
    preferredAZ: String,
    subnets: CacheSubnetGroup,
    securityGroup: SecurityGroup,
    tags: OTags
)

/** When I make the simple Redis, I can't find a way to get its endpoint.
  */
object Redis extends AWSUtils with AwsErrorUtils {
  lazy val client: ElastiCacheAsyncClient = ElastiCacheAsyncClient.builder().build()

  val toRedisTag: (String, String) => Tag = (k: String, v: String) => Tag.builder.key(k).value(v).build()

  // No Replicas, Single Zone AZ definately a dev setup
  // Actually, now only ss-controller use Redis, and they can be configured to use their own cache.
  // I guess the SOR is the set of ss-servers and they are essentially cache-ing stuff they scrape from there.
  // Anyway, you can add more nodes as needed.
  // I think just the node type is fixed.
  def create(conf: RedisConfig): IO[CacheCluster] = {
    // We will make a real Redis setup now.
    val rq = CreateCacheClusterRequest.builder
      .azMode(AZMode.SINGLE_AZ)                       // No cross-zone redundancy
      .engine("redis")                                // Not memcache
      .engineVersion("5.0.6")                         // Can skip to get latest?
      .autoMinorVersionUpgrade(true)                  // Allow upgrades
      .cacheClusterId(conf.name)                      // Cluster Name, can add N replicas
      .cacheNodeType(conf.nodeType)                   // Instance Type, e.g. cache.t3.small
      .cacheSubnetGroupName(conf.subnets.cacheSubnetGroupName)
      .numCacheNodes(1)                               // Redis has to be one
      .port(6379)                                     // Default
      .securityGroupIds(conf.securityGroup.groupId()) // Because making inside VPC
      .tags(conf.tags.via(toRedisTag))
      .build()
    // .replicationGroupId() // Not specicied so we have one stand-alone node.
    // Not sure how to enable encryption at rest or in-transit, maybe parameters group or option group

    IOU.toIO(client.createCacheCluster(rq)).map(_.cacheCluster)
  }

  def deleteRedis(cacheCluster: CacheCluster): IO[CacheCluster] = {
    completableFutureToIO(
      client.deleteCacheCluster(DeleteCacheClusterRequest.builder.cacheClusterId(cacheCluster.cacheClusterId()).build())
    ).map(_.cacheCluster)
  }

  def listReplicationGroups(): IO[Stream[IO, ReplicationGroup]] = {
    for {
      stream <- FS2Utils.toStream(client.describeReplicationGroupsPaginator())
      content = stream.map(_.replicationGroups().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  def listClusterCaches(): IO[Stream[IO, CacheCluster]] = {
    val rq = DescribeCacheClustersRequest.builder
      .showCacheNodeInfo(true)
      .showCacheClustersNotInReplicationGroups(true)
      .build()

    for {
      stream <- FS2Utils.toStream(client.describeCacheClustersPaginator(rq))
      content = stream.map(_.cacheClusters().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  /** This is only needed if Redis is outside the VPC? */
  def authorizeSecurityGroup(cacheSecurityGroupName: String, sgName: String, ownerAccount: String): IO[CacheSecurityGroup] = {
    IO(scribe.info(s"Authorign Security Group for Redis $sgName")) *>
      IOU
        .toIO(
          client.authorizeCacheSecurityGroupIngress(
            AuthorizeCacheSecurityGroupIngressRequest.builder
              .cacheSecurityGroupName(cacheSecurityGroupName)
              .ec2SecurityGroupName(sgName)
              .ec2SecurityGroupOwnerId(ownerAccount)
              .build()
          )
        )
        .map(_.cacheSecurityGroup)
  }

  def findClusterByName(name: String): IO[Option[CacheCluster]] = {
    listClusterCaches().flatMap { stream =>
      stream.filter(cc => cc.cacheClusterId.equals(name)).compile.last
    }
  }

  /** Not used or tested yet */
  def setNumberOfReplicas(count: Int): IO[ReplicationGroup] = {
    completableFutureToIO {
      client.increaseReplicaCount(
        IncreaseReplicaCountRequest.builder
          .newReplicaCount(count)
          //.replicationGroupId()  // Replication group to ADD to
          // rereplicaConfiguration()   // For clustering enabled
          .build()
      )
    }.map(_.replicationGroup())
  }

  /** IF resource doesn't exist return None, else a possible empty list of tags */
  def findTagsForCacheCluster(arn: String): IO[Option[List[Tag]]] = {
    IOU
      .toIO(client.listTagsForResource(ListTagsForResourceRequest.builder.resourceName(arn).build()))
      .redeem(AwsErrorUtils.nestedRecoverToOption[CacheClusterNotFoundException], rs => rs.tagList().asScala.toList.some)
  }

  def listSubnetGroups(): IO[Stream[IO, CacheSubnetGroup]] = {
    for {
      stream <- FS2Utils.toStream(client.describeCacheSubnetGroupsPaginator())
      content = stream.map(_.cacheSubnetGroups().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  def findSubnetGroupByName(name: String): IO[Option[CacheSubnetGroup]] = {
    for {
      stream   <- listSubnetGroups()
      filtered  = stream.filter(_.cacheSubnetGroupName().equals(name))
      optional <- filtered.compile.last
    } yield optional
  }

  def createCacheSubnetGroup(subnetIds: List[String], name: String, desc: String): IO[CacheSubnetGroup] = {
    IOU
      .toIO(
        client.createCacheSubnetGroup(
          CreateCacheSubnetGroupRequest.builder
            .cacheSubnetGroupName(name)
            .cacheSubnetGroupDescription(desc)
            .subnetIds(subnetIds.asJava)
            .build()
        )
      )
      .map(_.cacheSubnetGroup())
  }

  def getEndpointsByName(name: String): IO[List[Endpoint]] = {
    for {
      redis    <- findClusterByName(name) >>= IOU.required(s"Redis Named $name")
      endpoints = redis.cacheNodes().asScala.map(cnode => cnode.endpoint()).toList
    } yield endpoints
  }

  /** Assumes deleting is constant, may go into backup though? */
  def waitUntilDeleted(db: CacheCluster): IO[Unit] = {
    def cond(oc: Option[CacheCluster]): Boolean = oc.exists(v => v.cacheClusterStatus != "deleted")
    AWSUtils.pollWhile(cond)(findClusterByName(db.cacheClusterId())).void // Fing error on not there
  }

  /** Assumes deleting is constant, may go into backup though? */
  def waitUntilReady(db: CacheCluster): IO[DBCluster] = {
    def loopWhile(s: DBCluster) = s.status.equals("deleting")
    AWSUtils.pollWhile(loopWhile)(findCluster(db.cacheClusterId) >>= IOU.required(s"$db"))
  }

  def deleteRedisSubnetGroup(subnetGroupName: String): IO[Unit] = {
    findSubnetGroupByName(subnetGroupName).flatMap {
      case None    => IO(scribe.info(s"No Cache Subnet Group $subnetGroupName"))
      case Some(_) =>
        IOU.toIO(client.deleteCacheSubnetGroup(DeleteCacheSubnetGroupRequest.builder.cacheSubnetGroupName(subnetGroupName).build())).void
    }
  }
}
