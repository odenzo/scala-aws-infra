package com.odenzo.aws.rds

import cats._
import cats.data._
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._
import com.odenzo.aws.OTags
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{IOU, LoginCreds}
import software.amazon.awssdk.services.rds.RdsAsyncClient
import software.amazon.awssdk.services.rds.model._

/** SHould leverage ClusterOptionGroup and/or ClusterParameterGroups */
object RdsPostgres {

  val toPGTag: (String, String) => Tag = (k: String, v: String) => Tag.builder().key(k).value(v).build()

  lazy val client    = RdsAsyncClient.create()
  val smallInstance  = "db.t3.micro"
  val mediumInstance = "db.m4.large"
  val largeInstance  = "db.m5.large"

  /** This takes one subnet since no multi-AZ and only one instance
    * DB will be put in the first subnet of DBSubnetGroup
    */
  def createPostgresInstance(
      dbInstanceName: String,
      serverInstance: String,
      vpcSecurityGroupId: String,
      subnetGroup: DBSubnetGroup,
      admin: LoginCreds,
      tags: OTags
  )(implicit cs: ContextShift[IO]): IO[DBInstance] = {
    import scala.jdk.CollectionConverters._

    scribe.warn(s"Creating Postgres DB w/ master $admin    -- password can be reset via AWS Console")
    for {
      _  <- IO(scribe.info(s"SubnetGroup: ${oprint(subnetGroup)}"))
      az <- IO.fromOption(subnetGroup.subnets().asScala.headOption.map(_.subnetAvailabilityZone))(new Throwable("Impossible"))
      rq  = RdsPostgres.createDbRequest(dbInstanceName, serverInstance, az, vpcSecurityGroupId, subnetGroup, admin, tags)
      _   = scribe.info(s"Create DB Rq: ${oprint(rq)}")
      db <- createDb(rq)
    } yield db
  }

  def createDb(rq: CreateDbInstanceRequest)(implicit cs: ContextShift[IO]): IO[DBInstance] = {
    IOU.toIO(client.createDBInstance(rq)).map(_.dbInstance())
  }

  /** This creates a AWS Managed RDS Database with default Postgres Database. Not much point in stuffing down in AWS
    * layer since about every setting is viable for customization.
    */
  def createDbRequest(
      dbInstanceName: String,
      serverInstance: String,
      az: AvailabilityZone,
      vpcSecurityGroupId: String,
      subnetGroup: DBSubnetGroup,
      admin: LoginCreds,
      tags: OTags
  ): CreateDbInstanceRequest = {

    CreateDbInstanceRequest
      .builder()
      .engine("postgres")
      .engineVersion("11.8")
      .autoMinorVersionUpgrade(true)                    // Allow autoupgrade from 11.x => 11.x+1
      .allocatedStorage(20)                             // Starting Disk in GB
      .maxAllocatedStorage(100)                         // Auto-scale storage up as needed to max 100G
      .backupRetentionPeriod(7)                         // Days to keep each backup
      .copyTagsToSnapshot(true)                         // Yeah, tags are good
      .dbName("postgres")                               // Name of default DB
      .masterUsername(admin.user)                       // postgres and a secret by convention
      .masterUserPassword(admin.password.secret)        // Password for postgres su
      .port(5432)                                       // Use standard port
      .storageEncrypted(true)                           // ENcrypt with KMS key - not really needed I think
      .dbInstanceClass(serverInstance)                  // Size of each DB instance --
      .dbInstanceIdentifier(dbInstanceName)             // Each DB-server name (for RO instance or Multi-AZ fallback
      .availabilityZone(az.name())                      // Use the availability zone in the DBSubnetGroup
      .deletionProtection(false)                        // Let me delete is accidentally
      .publiclyAccessible(true)                         // Settable - but handy
      .storageType("gp2")                               // gp2 / standard / io1 if production with guarenteed IOPS
      .multiAZ(false)                                   // No redundant Availability Zone for small (yes for production)
      .enableIAMDatabaseAuthentication(false)           // We try and make AWS independant for now
      .vpcSecurityGroupIds(vpcSecurityGroupId)          // ok - this vs dbSecurityGroups and SubnetGroupName
      .dbSubnetGroupName(subnetGroup.dbSubnetGroupName) // Subnet to place the instance
      .enablePerformanceInsights(false)                 // Details thread level performance
      //.enableCloudwatchLogsExports("") // If DB logs stored to cloudwatch too.
      //.monitoringInterval(0) // Interval to do enhanced monitoring
      //.monitoringRoleArn()  // This role is required if doing enhanced monitoring
      .tags(tags.via(com.odenzo.aws.rds.RDS.toPGTag))
      .build()
  }

  /** Cleans up but error checking and waiting is WIP except exceptions
    * Return is optional if the original is not found. Otherwise, may need to check on progress.
    */
  def teardown(cluster: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    val instanceName = s"k8s-$cluster-instance"
    for {

      db <- RDS.findInstance(instanceName)
      _  <- db match {
              case Some(db) => RDS.deleteInstance(db).void
              case None     => IO(scribe.info(s"No Postgres DB $instanceName "))
            }
      _  <- db.traverse(RDS.waitUntilDbInstanceDeleted)

    } yield ()
  }

}
