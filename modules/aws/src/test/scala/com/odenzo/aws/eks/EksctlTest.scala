package com.odenzo.aws.eks

import cats.effect._
import com.odenzo.aws.CIDR
import com.odenzo.aws.ec2.{EC2, VPC}
import com.odenzo.aws.eks.eksctl.EksctlCommand
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.CommandLineArgs
import com.odenzo.utils.OPrint.oprint
import os.CommandResult

class EksctlTest extends AWSBaseTest {

  test("Generating eksctl command") {
    val cmd: IO[CommandResult] =
      EksctlCommand.buildCommand(
        "dev-playground",
        globalTags,
        region,
        List("us-east-1c", "us-east-1-d"),
        CIDR.fromStringUnsafe("10.2.0.0/16"),
        None
      )

  }

  test("Running eksctl for real") {
    val cmd: IO[CommandResult] =
      EksctlCommand.buildCommand(
        "dev-playground",
        globalTags,
        region,
        List("us-east-1c", "us-east-1-d"),
        CIDR.fromStringUnsafe("10.2.0.0/16"),
        None
      )

    scribe.debug(s"Command: ${oprint(cmd)}")

    cmd.attempt.unsafeRunSync() match {
      case Left(err) => scribe.error(s"Trouble Running\n", err); fail("Failed ksctl cmd", err)
      case Right(rs) =>
        scribe.info(s"Std Error: ${rs.out.toString}")
        scribe.info(s"Std Out: ${rs.err.toString}")
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
