package com.odenzo.aws.kafka

import cats._
import cats.data._
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, OTags}
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, OError, RetryableError}
import fs2._
import software.amazon.awssdk.services.ec2.model.{SecurityGroup, Subnet}
import software.amazon.awssdk.services.kafka.KafkaAsyncClient
import software.amazon.awssdk.services.kafka.model._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

object Kafka {
  lazy private val client: KafkaAsyncClient = KafkaAsyncClient.builder().build()

  // No Replicas, Single Zone AZ definately a dev setup
  // Actually, now only ss-controller use Redis, and they can be configured to use their own cache.
  // I guess the SOR is the set of ss-servers and they are essentially cache-ing stuff they scrape from there.

  /**
    *  Partial configuratin for a Kafka cluster, some stuff hard coded too.
    * @param name Cluster Name, verbatim
    * @param instanceType Machine type, e.g. kafka-m4-large
    * @param clientSubnets  Subnets to allow Kafka, note 2.4.1 rackspace to read from closest.
    * @param sg  VPC Security group to allow Kafka traffic
    * @param gbPerBroker   Disk storage per broker in GB
    * @param encryptInTransit true to encrypt data in transit. Always encrypt at rest. Client TLS or Plain
    * @param tags Standard Tags
    */
  case class KafkaConfig(
      name: String,
      instanceType: String,
      numBrokers: Int,
      gbPerBroker: Int,
      clientSubnets: NonEmptyList[Subnet],
      sg: List[SecurityGroup],
      encryptInTransit: Boolean,
      tags: OTags
  )

  protected def storagePerBroker(gbytes: Int): StorageInfo = {
    StorageInfo.builder
      .ebsStorageInfo(EBSStorageInfo.builder.volumeSize(gbytes).build)
      .build()
  }

  def encryptionInfo(inTransit: Boolean, atRest: Boolean): EncryptionInfo = {

    val builder: EncryptionInfo.Builder = EncryptionInfo.builder
      // Hmm, can specify key but what if I want default key
      // Think just skip .enctyptionAtRest if atRest == false
      .encryptionInTransit(
        EncryptionInTransit.builder
          .clientBroker(ClientBroker.TLS_PLAINTEXT)
          .inCluster(inTransit)
          .build()
      )

    val finalBuilder = if (atRest) builder.encryptionAtRest(EncryptionAtRest.builder.build) else builder
    finalBuilder.build()
  }

  protected def nodeGroup(config: KafkaConfig, storage: StorageInfo) = {

    BrokerNodeGroupInfo.builder
      .instanceType(config.instanceType)
      .clientSubnets(config.clientSubnets.toList.map(_.subnetId).asJavaCollection)
      .securityGroups(config.sg.map(_.groupId).asJavaCollection)
      .storageInfo(storage)
      .build()
  }

  def prometheusMonitoring(jmx: Boolean, node: Boolean): OpenMonitoringInfo = {
    OpenMonitoringInfo.builder
      .prometheus(
        PrometheusInfo
          .builder()
          .jmxExporter(JmxExporterInfo.builder().enabledInBroker(jmx).build())
          .nodeExporter(NodeExporterInfo.builder().enabledInBroker(node).build())
          .build()
      )
      .build()
  }

  def createClusterRq(config: KafkaConfig): IO[CreateClusterRequest] = {

    for {
      _             <- IO(scribe.info(s"Creating Kafka ${oprint(config)}"))
      storage        = storagePerBroker(config.gbPerBroker)
      encryption     = encryptionInfo(inTransit = false, atRest = true)
      nodegroup      = nodeGroup(config, storage)
      openMonitoring = prometheusMonitoring(jmx = true, node = true)
    } yield CreateClusterRequest.builder
      .clusterName(config.name)
      .numberOfBrokerNodes(config.numBrokers)
      .brokerNodeGroupInfo(nodegroup)
      .kafkaVersion("2.5.1") // Required, FIXME: how to find out latest and use that
      .encryptionInfo(encryption)
      .enhancedMonitoring(EnhancedMonitoring.DEFAULT)
      .openMonitoring(openMonitoring)
      .tags(config.tags.tags.asJava)
      .build()
  }

  /** Creates a cluster returning its name and ARN.
    * We then call get Cluster description to normaize usually
    */
  def createCluster(rq: CreateClusterRequest)(implicit cs: ContextShift[IO]): IO[CreateClusterResponse] = {
    IOU.toIO(client.createCluster(rq))
  }

  /** Takes a long while to actually delete */
  def deleteCluster(kafka: ClusterInfo)(implicit cs: ContextShift[IO]): IO[ClusterState] = {
    IOU
      .toIO(client.deleteCluster(DeleteClusterRequest.builder.clusterArn(kafka.clusterArn).build()))
      .map(_.state)
  }

  /** Gets the state. You may want stateinfo if it fails.
    * Now if failed I raise custom error with code and message.
    * This is mainly for looping to see when its done.
    */
  def checkClusterReady(kafkaArn: String)(implicit cs: ContextShift[IO]): IO[ClusterState] = {
    describeCluster(kafkaArn)
      .map { ci =>
        scribe.debug(s"Cluster State: ${ci.state()}")
        ci.state()
      }
      .reject {
//      case ClusterState.ACTIVE   => IO.pure(ClusterState.ACTIVE)
        case ClusterState.FAILED   => new Throwable(s"Cluster Creation Failed")
        case ClusterState.DELETING => new Throwable(s"Cluster being DELETED!")
        case ClusterState.CREATING => new RetryableError("Cluster being created")
        case ClusterState.UPDATING => new RetryableError("Cluster being updated")
      }
  }

  /** An example, usable. Check each minute is ACTIVE. If CREATING or UPDATING
    * it will keep checking, else return error if FAILED or DELETING or exceptional case
    * (e.g. Kafka instance not found by ARN)
    */
  def waitUntilClusterReady(kafkaArn: String)(implicit cs: ContextShift[IO], timer: Timer[IO]): IO[ClusterState] = {
    FS2Utils.uniformRetry(1 minute, 60)(checkClusterReady(kafkaArn))
  }

  /** Assumes deleting is constant, may go into backup though. Well, interesting thing
    * is if goes into ClusterState.FAILED instead not handled.
    */
  def waitUntilDeleted(clusterName: String)(implicit cs: ContextShift[IO]) = {
    def deletedComplete(st: Option[ClusterState]) =
      st match {
        case None                        => true
        case Some(ClusterState.DELETING) =>
          scribe.debug(s"Still Deleting Kafka $clusterName");
          false
        case Some(other)                 =>
          scribe.warn(s"Unexpected State in Deletion: $other")
          throw OError(s"Unexpected State in Deletion: $other")
      }
    AWSUtils.pollWhile(deletedComplete)(findCluster(clusterName).map(_.map(_.state())))
  }

  /** Can use this or just filter throgh listClusters, think same info.
    * This is better for looping to get building complete state etc.
    */
  def describeCluster(clusterArn: String)(implicit cs: ContextShift[IO]): IO[ClusterInfo] = {
    IOU
      .toIO(client.describeCluster(DescribeClusterRequest.builder().clusterArn(clusterArn).build()))
      .map(_.clusterInfo())
  }

  /** Return a fs2 of unfiltered Kafka clusters */
  def listClusters()(implicit cs: ContextShift[IO]): IO[Stream[IO, ClusterInfo]] = {
    for {
      stream <- FS2Utils.toStream(client.listClustersPaginator())
      content = stream.map(_.clusterInfoList().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  def findCluster(clusterName: String)(implicit cs: ContextShift[IO]): IO[Option[ClusterInfo]] = {
    for {
      clusterStream <- listClusters()
      cluster       <- clusterStream.filter(_.clusterName.equals(clusterName)).compile.last

    } yield cluster
  }

  def getBootstrapBrokers(clusterArn: String)(implicit cs: ContextShift[IO]): IO[GetBootstrapBrokersResponse] = {
    IOU.toIO {
      client.getBootstrapBrokers(GetBootstrapBrokersRequest.builder().clusterArn(clusterArn).build)
    }
  }

  def getZookeepers(clusterArn: String)(implicit cs: ContextShift[IO]): IO[String] = {
    describeCluster(clusterArn).map(_.zookeeperConnectString())

  }
}
