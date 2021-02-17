package com.odenzo.aws.eks

import cats._
import cats.data._
import cats.syntax.all._
import com.odenzo.aws.OTags
import software.amazon.awssdk.services.ec2.model.{SecurityGroup, Subnet}
import software.amazon.awssdk.services.eks.model._

import java.util.UUID
import scala.jdk.CollectionConverters._

object EksRequests {

  /**
    * @param subnets  Subnets for Cluster workers, properly tagged I guess
    * @param securityGroups Primary and Sdcondary Security Groups for the Cluster
    * @return
    */
  def createVpcConfig(subnets: List[Subnet], securityGroups: List[SecurityGroup]): VpcConfigRequest = {
    VpcConfigRequest.builder
      .publicAccessCidrs("0.0.0.0/0")                         // Must also allow Fargate if restricted
      .endpointPrivateAccess(true)
      .endpointPublicAccess(true)
      .securityGroupIds(securityGroups.map(_.groupId).asJava) // SG for ENI for worker/control plan communication
      .subnetIds(subnets.map(_.subnetId()).asJavaCollection)  // Subnets to add ENI to, these have to be tagges correctly per AWS EKS docs
      .build()
  }

  /** Use KMS to encrypt the K8s secrets, good if transperant... to test not currently used. */
  def createEncryption(): EncryptionConfig = {
    EncryptionConfig.builder
      .resources("secrets")
      .provider(Provider.builder().keyArn("aws/kafka").build()) // alias doens't work, docs wrong
      .build()

  }

  def logControlPlaneToCloudwatch(yes: Boolean) = {
    val enabled = LogSetup.builder.enabled(yes).types(LogType.AUDIT).build
    Logging.builder.clusterLogging(enabled).build
  }

  /* My Better sense says just use eksctl via command line, but... */
  def createClusterRq(
      clusterName: String,
      subnets: List[Subnet],
      roleArn: String,
      securityGroup: SecurityGroup,
      tags: OTags
  ): CreateClusterRequest = {
    val requestToken = UUID.randomUUID().toString

    CreateClusterRequest.builder
      .name(clusterName)
      .clientRequestToken(requestToken)
      //.version()     // Use the latest version if not specific
      .roleArn(roleArn) // Required
      .resourcesVpcConfig(createVpcConfig(subnets, List(securityGroup)))
      .logging(logControlPlaneToCloudwatch(true))
      // .encryptionConfig(createEncryption())
      .tags(tags.tags.asJava)
      .build()
  }
}
