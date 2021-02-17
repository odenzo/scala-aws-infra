package com.odenzo.aws.iam

import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.ScribeControl
import com.odenzo.aws.eks.{ClusterServiceRole, NodeInstanceRole}
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.iam.model.{ServiceSpecificCredentialMetadata, User}

class IAMTest extends AWSBaseTest {

  val aRole = "K8S-eek6-LambdaManagement"
  test("List Role Policies") {
    val policies = IAM.listRolePolicies(aRole).flatMap(_.compile.toList).unsafeRunSync()
    val attached = IAM.listRoleAttachedPolicies(aRole).flatMap(_.compile.toList).unsafeRunSync()
    scribe.debug(s"Policies: \n ${oprint(policies)} ")
    scribe.debug(s"Attached Policies: \n ${oprint(attached)} ")
  }

  test("Fully Delete Role") {
    IAM.deleteRoleAndPoliciesIfExists(aRole).unsafeRunSync()
  }
  test("Streaming List Users Filtered") {
    ScribeControl.sush()
    val prog: IO[Vector[User]] = for {
      stream    <- IAM.listUsers()
      streamOut <- stream.filter((u: User) => !u.path.startsWith("/AWS/OpsWorks/")).compile.toVector
    } yield streamOut
    val users                  = prog.unsafeRunSync()
    scribe.info(s"Users: + ${oprint(users)}")
  }

  test("List Cassandra Acess") {
    ScribeControl.sush()
    val serviceName                                  = "cassandra.amazonaws.com"
    val ran: List[ServiceSpecificCredentialMetadata] =
      IAM.listServiceSpecificCredentials("SteveFranks", serviceName).unsafeRunSync()

    scribe.info(s"Credentials ${oprint(ran)}")
    ran.head
  }

  test("Create a Policy") {
    ScribeControl.sush()
    val prog = for {
      _            <- IO(scribe.info("Testing Combining JSON Policy Stuff"))
      servicePolicy = ClusterServiceRole.nlb
      s3Policy      = NodeInstanceRole.externalDNSChangeSet
      policies      = IAMUtils.combinePolicies(servicePolicy, s3Policy)
      policy       <- IAM.createPolicy("aaaa-deleteme", "/k8s/testing/", "Sample Policy From JSON", policies)
    } yield policy

    val res = prog.unsafeRunSync()
    scribe.info(s"Result: ${oprint(res)}")
  }

  test("Dev Create Role and Attach Policies etc") {
    ScribeControl.sush()
    val prog   = for {
      role <- IAM.createRole("installer-test", "aaaa-test-role", TrustRelationships.rootAdmin, globalTags)
    } yield role
    val result = prog.unsafeRunSync()
    scribe.info(s"Role ${oprint(result)}")
  }

//  test("Create Role at Attach AWS and Customer Policy") {
//    ScribeControl.sush()
//    val prog   = for {
//      role          <- IAM.createRole("installer-test", "aa-test-role", TrustRelationships.rootAdmin, globalTags)
//      servicePolicy <- RawPolicies.clusterServicesPolicy
//      s3Policy      <- RawPolicies.workersPolicyRoute53ChangeSet
//      policies       = RawPolicies.combinePolicies(servicePolicy, s3Policy)
//      policy        <- IAM.createPolicy("aa-deleteme", "/k8s/testing/", "Sample Policy From JSON", policies)
//      _             <- IAM.attachRolePolicy(role.roleName, policy.arn)
//      mpolicy       <- IAM.findNamedPolicy("AmazonElastiCacheFullAccess") >>= IOUtils.required(s"Policy Not Found")
//      _              = scribe.info(s"Found Redis ${oprint(mpolicy)}")
//      _             <- IAM.attachRolePolicy(role.roleName, mpolicy.arn)
//    } yield role
//    val result = prog.unsafeRunSync()
//    scribe.info(s"Role ${oprint(result)}")
//  }
}
