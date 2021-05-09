package com.odenzo.aws.ec2

import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.ec2.model.SecurityGroup

class SecurityGroupsTest extends AWSBaseTest {

  test("Look for Security Group") {
    val vpcId                        = "vpc-0a74ce726c3026565"
    val vpc                          = VPC.getVpcById(vpcId).unsafeRunSync()
    scribe.info(s"VPC ${oprint(vpc)}")
    assertEquals(vpc.vpcId(), vpcId)
    val found: Option[SecurityGroup] = SecurityGroups.findSecurityGroupByNameInVpc("k8s/installer-test/postgres", vpc).unsafeRunSync()
    scribe.info(s"Found: ${oprint(found)}")
  }

  test("Add a rule twice with no erro") {
    val sgId = "sg-020e35a68e2db5106"
    val prog = for {
      sg   <- SecurityGroups.getSecurityGroupById(sgId)
      _    <- SecurityGroups.addOutboundRules(sg, List(SecurityGroupHelpers.allTcpPermissions))
      _    <- SecurityGroups.addOutboundRules(sg, List(SecurityGroupHelpers.allTcpPermissions))
      sgUp <- SecurityGroups.getSecurityGroupById(sgId)
    } yield sgUp
    val res  = prog.unsafeRunSync()
    scribe.info(s"Results $res")
  }
}
