package com.odenzo.aws.eks.eksctl

import cats._
import cats.data.{NonEmptyList, _}
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.{CIDR, OTags}
import com.odenzo.utils.CommandLine
import com.odenzo.utils.CommandLine.{Args, BinaryArg, Command, Executor}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.ec2.model.Subnet

import java.io.File

/** eksstl building of a cluster. Note this requires eksctl is setup already for the machien running this.
  */
object EksctlCommand {

  // Meh, appending so in reverse order
  val createCluster: Args = Args.empty.add("create").add("cluster")

  def baseArgs(cluster: String, region: Region): Args =
    Args.empty
      .add("--name", cluster)          // Name of the cluster, DNS style, keep short
      .add("--region", region.toString)
      .add("--full-ecr-access")        // Docker Registry
      .add("--asg-access")             // Auto-Scaler
      .add("--external-dns-access")    // Auto-DNS update of Route53
      .add("--set-kubeconfig-context") // Automatically set the current kubectl context to created cluster
      .add("--auto-kubeconfig")        // Automatically write .kube config

  def withNoNodeGroup: Args = Args.empty.add("--without-nodegroup")

  def withNodegroupSmall(name: String = "workers"): Args = {
    Args.empty
      .add("--managed")         // EKS Creates the Managed Node Group of Workers
      .add("--version", "auto") // Kubernetes Version - omit for latest  1.17 but eksctl outdates
      .add("--nodegroup-name" -> name) // Name of initial EC2 worker group
      .add("--node-type" -> "t3.large")
      .add("--nodes" -> "2") // Number of worker nodes to start with
      .add("--nodes-min" -> "1") // Min
      .add("--nodes-max" -> "3") // Max, either manually or auto-scaled
      .add("--node-volume-size" -> "20") // Size in GB of local disk
      // .add("--node-volume-type", "gp2")   // Not supported for managed node groups
      // .add("--node-security-groups") // Extra Security Groups
      //.add("--node-zone", nodeZones.mkString(","))
      .add("--instance-prefix", "k8s/")

  }

  def withVerbosity(level: Int = 4): Args = {
    Args.empty.add("--verbose", level.toString)
  }

  /** Allows ssh into EC2 nodes using named existing EC2 keypair, usually a PEM */
  def withSshAccessWithKeyPair(keypairName: String): Args =
    Args.empty.add("--ssh-access").add("--ssh-public-key", keypairName)

  /** Allows ssh into EC2 nodes using SSH public key contents. Not sure what type required */
  def withSshAccess(keyFile: File): Args =
    Args.empty
      .add("--ssh-access")                              // Want SSH Access to EC2 runnign containers?
      .add("--ssh-public-key", keyFile.getAbsolutePath) // Then give a public key file

  /** Common Tags that all created (mostly) cluster objects will get */
  def withTags(tags: OTags): Args = {
    val tagList = tags.tags.iterator.map(kv => s"${kv._1}=${kv._2}").mkString(",")
    Args.empty
      .add("--tags", tagList) // Add *additional* tags to cluster objects
    // .add("--node-label", tagList) // K8s Labels
  }

  /** Kubernetes Labels --  TODO: these will be normalized since keys follow different rules */
  def withKubernetesLabels(tags: OTags): Args = {
    val tagList = tags.tags.iterator.map { case (k, v) => s"$k=$v" }.mkString(",")
    Args.empty
      .add("--node-label", tagList) // K8s Labels
  }

  /**
    * @param zones    List of zones in region, us-east-1a, us-east-1b style
    * @return
    */
  def withNewVPC(zones: List[String], vpcCidr: CIDR): Args = {
    Args.empty
      .add("--vpc-cidr", vpcCidr.toString) // CIDR of VPC to make
      .add("--zones", zones.mkString(",")) // Availability Zones -- 2 enough usually us-east-1 us-east-2
      .add("--vpc-nat-mode", "Single")
  }

  /** subnetLists can be empty  -- this is still failing. Try with empty private subnets. */
  def withSubnets(publicSubnets: List[Subnet], privateSubnets: List[Subnet]): Args = {
    val pubSubnets  = NonEmptyList.fromList(publicSubnets.map(_.subnetId)).map(_.mkString_(","))
    val privSubnets = NonEmptyList.fromList(privateSubnets.map(_.subnetId)).map(_.mkString_(","))

    // NAT for private subnets - unoptimized, add even if no private subnets for other VPC usage
    Args(
      List(
        BinaryArg("--vpc-nat-mode", "Single").some,
        pubSubnets.map(BinaryArg("--vpc-public-subnets", _)),
        privSubnets.map(BinaryArg("--vpc-private-subnets", _))
      ).flatten
    )
  }

  /** Lets build with new VPC and supply subnets to see what happens */
  def buildCommand(
      clusterName: String,
      tags: OTags,
      region: Region,
      zones: List[String],
      cidr: CIDR,
      sshPubKey: Option[String]
  ): Command = {

    val args = createCluster
      .add(baseArgs(clusterName, region))
      .add(withNodegroupSmall())
      .add(withNewVPC(zones, cidr))
      .add(withTags(tags))
      .add(sshPubKey.map(withSshAccessWithKeyPair))
      .add(withVerbosity(5))
    Command("eksctl", args)
  }

  /** Trying to build in excisting VPC by supplying the subnets,by ID. subnets reference the VPC */
  def buildInVpcWithNodegroup(
      clusterName: String,
      publicSubnets: List[Subnet],
      privateSubnets: List[Subnet],
      tags: OTags,
      region: Region,
      sshPubKey: Option[File]
  ): Command = {
    val args = createCluster
      .add(baseArgs(clusterName, region))
      .add(withNodegroupSmall())
      .add(withSubnets(publicSubnets, privateSubnets))
      .add(withTags(tags))
      .add(sshPubKey.map(withSshAccess))

    Command("eksctl", args)
  }
  def executeComand(cmdLine: Command): IO[CommandLine.CmdLineResult] = {

    Executor.run(cmdLine)
  }

  def executeFile(yaml: File): Command = {
    Command(s"eksctl create cluster -f ${yaml.getAbsolutePath}")
  }

  def updateKubeConfig(cluster: String): IO[CommandLine.CmdLineResult] = {
    val cmd = Command(s"eksctl", Args.empty.add("utils").add("write-kubeconfig").add(cluster))
    Executor.run(cmd)
  }
}
