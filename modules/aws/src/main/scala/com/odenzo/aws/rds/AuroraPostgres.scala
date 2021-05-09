package com.odenzo.aws.rds

import cats.effect.IO
import com.odenzo.aws.OTags
import com.odenzo.utils.{IOU, LoginCreds}
import software.amazon.awssdk.services._
import software.amazon.awssdk.services.ec2.model.SecurityGroup
import software.amazon.awssdk.services.rds.model._

import scala.jdk.CollectionConverters._

/** SHould leverage ClusterOptionGroup and/or ClusterParameterGroups */
object AuroraPostgres {

  case class ScalingParams(min: Int = 2, max: Int = 32, autoPause: Boolean = true, pauseDelaySec: Int = 120)

  /** Deletes the DB cluster, but not the SubnetGroup or Security Groups */
  def teardown(dbClusterName: String): IO[Unit] = {
    val req = DeleteDbClusterRequest.builder.dbClusterIdentifier(dbClusterName).skipFinalSnapshot(true).build()
    RDS.findCluster(dbClusterName).flatMap {
      case Some(db) =>
        IOU.toIO(RDS.client.deleteDBCluster(req)).map(_.dbCluster) *> RDS.waitUntilDbClusterDeleted(db).void
      case None     => IO(scribe.info(s"No DB Cluster $dbClusterName to delete"))
    }
  }

  def createAuroraServerless(
      dbClusterName: String,
      admin: LoginCreds,
      dbSubnetGroup: DBSubnetGroup,
      securityGroup: SecurityGroup,
      scalingParams: ScalingParams,
      tags: OTags
  ): IO[DBCluster] = {
    val az                         = dbSubnetGroup.subnets().asScala.map(sn => sn.subnetAvailabilityZone().name()).toList.distinct
    val rq: CreateDbClusterRequest = rds.model.CreateDbClusterRequest
      .builder()
      .backupRetentionPeriod(14)
      .availabilityZones(az.asJavaCollection)
      // .characterSetName("UTF-8") // Can't change
      .copyTagsToSnapshot(true)
      .databaseName("postgres")
      .dbClusterIdentifier(dbClusterName)
      .dbSubnetGroupName(dbSubnetGroup.dbSubnetGroupName())
      .deletionProtection(false)
      .engine("aurora-postgresql")
      .engineVersion("10.7")
      .engineMode("serverless")
      .scalingConfiguration(
        ScalingConfiguration
          .builder()
          .autoPause(scalingParams.autoPause)
          .maxCapacity(scalingParams.max)                     // Capacity Units
          .minCapacity(scalingParams.min)                     // Capacity Units
          .secondsUntilAutoPause(scalingParams.pauseDelaySec) // Pause after 120 seconds
          .build()
      )
      .masterUsername(admin.user)
      .masterUserPassword(admin.password.secret)
      .port(5432)
      .storageEncrypted(true)
      .vpcSecurityGroupIds(List(securityGroup).map(_.groupId()).asJavaCollection)
      .tags(tags.via(RDS.toPGTag))
      .build()
    IOU.toIO(RDS.client.createDBCluster(rq)).map(_.dbCluster())
  }

}
