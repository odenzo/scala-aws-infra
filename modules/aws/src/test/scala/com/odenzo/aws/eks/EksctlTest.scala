package com.odenzo.aws.eks

import cats._
import cats.data._
import cats.effect._
import cats.effect.syntax.all._
import com.odenzo.aws.CIDR
import com.odenzo.aws.ec2.{EC2, VPC}
import com.odenzo.aws.eks.eksctl.EksctlCommand
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.CommandLine
import com.odenzo.utils.OPrint.oprint

class EksctlTest extends AWSBaseTest {

  test("Generating eksctl command") {
    val cmd: CommandLine.Command =
      EksctlCommand.buildCommand(
        "dev-playground",
        globalTags,
        region,
        List("us-east-1c", "us-east-1-d"),
        CIDR.fromStringUnsafe("10.2.0.0/16"),
        None
      )

    scribe.info(s"Command: ${cmd}")

    scribe.info(s"ZSH: \n ${cmd.toZsh}")
  }

  test("Running eksctl for real") {
    val cmd: CommandLine.Command =
      EksctlCommand.buildCommand(
        "dev-playground",
        globalTags,
        region,
        List("us-east-1c", "us-east-1-d"),
        CIDR.fromStringUnsafe("10.2.0.0/16"),
        None
      )

    scribe.debug(s"Command: ${oprint(cmd)}")

    EksctlCommand.executeComand(cmd).attempt.unsafeRunSync() match {
      case Left(err) => scribe.error(s"Trouble Running\n", err); fail(err)
      case Right(rs) =>
        scribe.info(s"Std Error: ${rs.stderr}")
        scribe.info(s"Std Out: ${rs.stdout}")
      // The bitch abount this is needs manually inspection, but we know clustername so we can API search anyway

    }
  }

  test("Getting Instances in NodeGroup") {
    val cluster = "dev"
    val prog    = for {
      vpc      <- VPC.getVpcByClusterName(cluster)
      ng       <- EKS.findNodegroup(cluster, "ss-servers")
      allNodes <- EC2.describeInstances(EC2.vpcFilter(vpc))
      _        <- IO(scribe.info(s"AllNodes: ${oprint(allNodes)}"))
    } yield (ng)
    val res     = prog.unsafeRunSync()
    scribe.debug(s"NodeGroup: ${oprint(res)}")

  }
}

object EksctlTest {}
