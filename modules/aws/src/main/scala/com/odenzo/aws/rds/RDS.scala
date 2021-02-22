package com.odenzo.aws.rds

import cats._
import cats.data._
import cats.effect.{ContextShift, IO}
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, OTags}
import com.odenzo.utils.{FS2Utils, IOU, OError}
import fs2._
import software.amazon.awssdk.services._
import software.amazon.awssdk.services.rds.RdsAsyncClient
import software.amazon.awssdk.services.rds.model._

import scala.jdk.CollectionConverters._

/** This has the common support for dealing with Postgres. AuroroaPostgres and RDS_Postgres hace specific functions
  * depending which ones using.
  */
object RDS extends AWSUtils {

  val toPGTag: (String, String) => Tag = (k: String, v: String) => Tag.builder().key(k).value(v).build()

  lazy val client = RdsAsyncClient.create()

  /** Create a DB Subnet Group with  at least two  subnets in different zones, names and tags
    * What if I don't want different zones -- I have to specify a subgroup
    */
  def createDbSubnetGroupIfMissing(subnetIds: List[String], name: String, desc: String, tags: OTags)(
      implicit cs: ContextShift[IO]
  ): IO[DBSubnetGroup] = {
    val subnetGroupName = name.replace('/', '-')
    findDbSubnetGroup(subnetGroupName).flatMap {
      case Some(sg) =>
        IO(scribe.info(s"Subnet Group $subnetGroupName already exists, not re-creating but probably should")).as(sg)

      case None     =>
        IOU
          .toIO {
            client.createDBSubnetGroup(
              CreateDbSubnetGroupRequest.builder
                .dbSubnetGroupName(subnetGroupName)
                .dbSubnetGroupDescription(desc)
                .subnetIds(subnetIds.asJavaCollection)
                .tags(tags.via(toPGTag))
                .build()
            )
          }
          .map(_.dbSubnetGroup())
    }
  }

  /** No matching group fails :-( */
  def findDbSubnetGroup(name: String)(implicit cs: ContextShift[IO]): IO[scala.Option[DBSubnetGroup]] = {
    for {
      stream  <- listDbSubnetGroups()
      filtered = stream.filter(_.dbSubnetGroupName().equals(name))
      oneOpt  <- filtered.compile.last
    } yield oneOpt
  }

  def listDbSubnetGroups()(implicit cs: ContextShift[IO]): IO[Stream[IO, DBSubnetGroup]] = {
    for {
      stream  <- FS2Utils.toStream(client.describeDBSubnetGroupsPaginator())
      contents = stream.map(_.dbSubnetGroups().asScala.toList) >>= Stream.emits
    } yield contents
  }

  /** Deletes the DbSubnetGroup iff it exists. Throws error if it doesn */
  def deleteDbSubnetGroup(name: String)(implicit cs: ContextShift[IO]): IO[String] = {
    IOU
      .toIO(client.deleteDBSubnetGroup(DeleteDbSubnetGroupRequest.builder.dbSubnetGroupName(name).build))
      .as("Deleted")
  }

  /** On Pending success returns the original dbsg (no status updated) */
  def deleteDbSubnetGroup(dbsg: DBSubnetGroup)(implicit cs: ContextShift[IO]): IO[DBSubnetGroup] = {
    IOU
      .toIO(client.deleteDBSubnetGroup(DeleteDbSubnetGroupRequest.builder.dbSubnetGroupName(dbsg.dbSubnetGroupName).build))
      .as(dbsg)
  }

  /** This copies over EC2 security group inbound rules */
  def addInboundSecurityGroupRules(sg: DBSecurityGroup, ec2securityGroupId: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    IOU.toIO {
      client.authorizeDBSecurityGroupIngress(
        AuthorizeDbSecurityGroupIngressRequest.builder
          .dbSecurityGroupName(sg.dbSecurityGroupName)
          .ec2SecurityGroupId(ec2securityGroupId)
          //.cidrip()
          .build()
      )
    }.void
  }

  def describeClusters()(implicit cs: ContextShift[IO]): IO[Stream[IO, DBCluster]] = {
    val request = rds.model.DescribeDbClustersRequest.builder().build()
    for {
      stream <- FS2Utils.toStream(client.describeDBClustersPaginator(request))
      content = stream.map(_.dbClusters().asScala.toList)
      burst   = content.flatMap(i => Stream.emits(i)).covary[IO]
    } yield burst
  }

  def getCluster(dbClusterName: String)(implicit cs: ContextShift[IO]): IO[DBCluster] = {
    findCluster(dbClusterName) >>= IOU.required(s"DB Cluster $dbClusterName")
  }

  /** Non Paginating with max 100 returns.https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBClusters.html */
  def findClusters(filters: Filter*)(implicit cs: ContextShift[IO]): IO[List[DBCluster]] = {
    IOU
      .toIO(client.describeDBClusters(DescribeDbClustersRequest.builder.filters(filters.asJavaCollection).maxRecords(100).build()))
      .map(v => fromJList(v.dbClusters()))
  }

  def findCluster(dbClusterName: String)(implicit cs: ContextShift[IO]): IO[scala.Option[DBCluster]] = {
    // cluster-id is actually the only filter
    findClusters(Filter.builder.name("db-cluster-id").values(dbClusterName).build()) >>= IOU.optionOne(s"DB Cluster Id $dbClusterName")
  }

  def findInstance(instanceName: String)(implicit cs: ContextShift[IO]): IO[scala.Option[DBInstance]] = {
    for {
      stream <- describeInstances()
      db     <- stream.filter(_.dbInstanceIdentifier.equals(instanceName)).compile.last
    } yield db
  }

  /** Also used to update the instance state */
  def describeInstance(db: DBInstance)(implicit cs: ContextShift[IO]): IO[DBInstance] = {
    findInstance(db.dbInstanceIdentifier) >>= IOU.required(s"DB Instance Not Found ${db.dbInstanceIdentifier}")
  }

  /** Status of DB Instance or None if not found (i.e. Deleted) */
  def findInstanceStatus(db: DBInstance)(implicit cs: ContextShift[IO]): IO[scala.Option[String]] = {
    findInstance(db.dbInstanceIdentifier()).map(opt => opt.map(_.dbInstanceStatus()))
  }

  /** Streams all the DB instances, not sure what happens if none, should be empty list I presume */
  def describeInstances()(implicit cs: ContextShift[IO]): IO[Stream[IO, DBInstance]] = {
    for {
      stream <- FS2Utils.toStream(client.describeDBInstancesPaginator())
      content = stream.map(_.dbInstances.asScala.toList) >>= Stream.emits
    } yield content

  }

  /** Deletes an instance and its backup with no final snapshot.
    * This may take a while to delete
    */
  def deleteInstance(db: DBInstance)(implicit cs: ContextShift[IO]): IO[DBInstance] = {
    IO(scribe.info(s"Deleting Postgres Instance $db")) *>
      IOU
        .toIO(
          client.deleteDBInstance(
            DeleteDbInstanceRequest.builder
              .dbInstanceIdentifier(db.dbInstanceIdentifier)
              .deleteAutomatedBackups(true)
              .skipFinalSnapshot(true)
              .build
          )
        )
        .map(_.dbInstance)
  }

  /** Assumes deleting is constant, may go into backup though? */
  def waitUntilDbInstanceDeleted(db: DBInstance)(implicit cs: ContextShift[IO]): IO[Unit] = {
    def stillDeleting(s: scala.Option[String]): Boolean =
      s match {
        case Some("deleting") => true
        case Some(other)      => throw OError(s"Unexpected DB Instance Status when deleting $other")
        case None             => false
      }
    AWSUtils.pollWhile(stillDeleting)(findInstanceStatus(db)).void
  }

  /** Assumes deleting is constant, may go into backup though? */
  def waitUntilDbClusterDeleted(db: DBCluster)(implicit cs: ContextShift[IO]): IO[Unit] = {
    def loopWhile(s: String) = s.equals("deleting")

    AWSUtils.waitWhile(loopWhile)(findCluster(db.dbClusterIdentifier()).map(_.map(_.status())))
  }

  /** This is used as a "waitor" to see if an issued create/delete/modify has completed (error or not) */
  def checkInstanceReady(dbToCheck: DBInstance)(implicit cs: ContextShift[IO]): IO[DBInstance] = {
    // Non-Enumerated Status so setting here as a Doc https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html
    val inProgress =
      Set("starting", "stopping", "upgrading", "modifying", "renaming", "backing-up", "upgrading", "rebooting", "creating", "deleting")

    def keepPolling(v: DBInstance) = inProgress.contains(v.dbInstanceStatus.toLowerCase)
    AWSUtils.pollWhile(keepPolling)(RDS.describeInstance(dbToCheck) <* IO(scribe.debug(s"Checking DB $dbToCheck")))

  }

  /** This is used as a "waitor" to see if an issued create/delete/modify has completed (error or not) */
  def checkClusterReady(db: DBCluster)(implicit cs: ContextShift[IO]): IO[DBCluster] = {
    // Non-Enumerated Status so setting here as a Doc https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Status.html
    val inProgress                = Set("starting", "stopping", "upgrading", "modifying", "renaming", "upgrading", "rebooting", "creating", "deleting")
    def keepPolling(v: DBCluster) = inProgress.contains(v.status().toLowerCase)
    AWSUtils.pollWhile(keepPolling)(RDS.getCluster(db.dbClusterIdentifier()))
  }

}
