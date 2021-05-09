package com.odenzo.aws.ec2

import cats.data._
import cats.effect.{IO, Resource}
import cats.syntax.all._
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.aws.{AwsPEM, ScribeControl}
import com.odenzo.utils.IOU
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.ec2.model.{Instance, IpRange, SecurityGroup, Vpc}

import java.io.{File, FileWriter}
import java.nio.charset.Charset

class EC2Test extends AWSBaseTest {
  ScribeControl.sush()
  test("Describe VPC") {
    val vpcs: List[Vpc] = VPC.describeVPCs().flatMap(_.compile.toList).unsafeRunSync()
    scribe.info(s"All VPC: ${oprint(vpcs)}")

  }

  test("List Images") {
    val images = EC2.listAwsAmi().unsafeRunSync().filter { i => Option(i.description).fold(false)(d => d.contains("buntu")) }
    scribe.info(s"AWS AMI Images: \n ${oprint(images)}")
  }

  test("Create Security Group") {

    val vpcId = "vpc-0fa6293406722b624"

    val prog = for {
      vpc   <- VPC.getVpcById(vpcId)
      helper = new VPCHelpers(vpc)

      in  = List(
              helper.buildTcpPermission(5432, helper.ipRangeVpc("Postgres in VPC")),
              helper.buildTcpPermission(5432, IpRange.builder().cidrIp("233.0.0.0/16").description("Home Access").build())
            )
      out = List(helper.buildTcpPermission(9999, IpRange.builder().cidrIp("233.33.0.0/24").description("Junk").build()))

      sgA  <- SecurityGroups.constructSecurityGroupIfNeeded("foo", "bar", vpc, in, out)
      _    <- EC2.tagResources(List(sgA.groupId), globalTags.modifyName(_ + s"/cf1"))
      upsg <- SecurityGroups.getSecurityGroupById(sgA.groupId())
    } yield upsg

    val res = prog.attempt.unsafeRunSync()
    scribe.info(s"Result ${oprint(res)}")

  }

  test("Find Security Group") {
    val results: SecurityGroup = SecurityGroups.getSecurityGroupById("sg-0463443e3ce701e3e").unsafeRunSync()
    scribe.info(s"Results $results")
  }

  test("Launch EC2 Instance") {
    val vpcId    = "vpc-0a74ce726c3026565"
    val keyName  = "k8s-installer-test-admin"
    val subnetId = "subnet-04c2040da79cbf505"
    val ami      = "ami-039a49e70ea773ffc"

    val prog          = for {
      vpc    <- VPC.getVpcById(vpcId)
      _      <- EC2.findKeyPairByName(keyName) >>= IOU.required(s"KeyPair not found for Name $keyName")
      subnet <- Subnets.findSubnetById(subnetId) >>= IOU.required(s"Subnet $subnetId not found")
      sg     <- SecurityGroups.getOrCreateSecurityGroup("k8s/installer-test/cf", "Aggregate Security Group for CF EC2 Nodes", vpc)
      rs     <- EC2.createAndRunInstance(keyName, subnet, ami, "t3.small", NonEmptyList.one(sg), globalTags.modifyName(_ + "/cf1"))
    } yield rs
    val res: Instance = prog.unsafeRunSync()
    scribe.info(s"Result: Status ${res.state()} -${res.stateReason()} \n ${oprint(res)}")
  }

  test("Create KeyPair and Write to Disk SSH Style") {
    val keyMatter: IO[Unit] = EC2
      .createKeyPair("test-to-delete", globalTags)
      .map(AwsPEM.from)
      .map { pem =>
        scribe.debug(s"Name: ${pem.name}  ID: ${pem.id}")
        scribe.info(s"Key Material\n${pem.material.secret}\n")
        pem.material.secret
      }
      .flatMap { key =>
        val toFile = new File("test.pem")
        Resource
          .make {
            IO(new FileWriter(toFile, Charset.forName("UTF-8")))
          } { src => IO(src.close()).handleErrorWith(_ => IO.unit) }
          .use(dest => IO(dest.write(key)))
      }

    keyMatter.unsafeRunSync()
  }
}
