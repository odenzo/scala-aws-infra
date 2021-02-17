package com.odenzo.aws.eks

import cats.effect.IO
import com.odenzo.aws.ScribeControl
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.eks.model.Cluster

class EKSTest extends AWSBaseTest {
  ScribeControl.sush()

  test("Describe Existing Cluster") {
    scribe.info("Building Lazy Request")
    val rs: IO[Cluster] = EKS.describeCluster("horn-k8s-RC2")
    scribe.info("Done Building and Taking a Nap")
    Thread.sleep(10000)
    scribe.info("AWAKE!")
    val cluster         = rs.unsafeRunSync()
    scribe.info(s"Cluster Result: ${oprint(cluster)}")
  }

  /** Incremental dev... */
  test("Create a Cluster") {
//    val rq          = EksRequests.createClusterRq("foobar")
//    val rs: Cluster = EKS.executeCreateCluster(rq).unsafeRunSync()
//    scribe.info(s"Cluester Response ${oprint(rs)}")
  }

  test("Describe Nogegroup") {
    // Want to be able to drill down to see why a failed  create nodegroup failed
    EKS.describeNodegroup("installer-test", "")
  }

  test("Attach Cluster Services Roles/Policy") {
    EKS.attachStandardEksServiceRolePolicies("k8s-installer-test-service").unsafeRunSync()
  }

  test("Attach Cluster Nodegroup Roles/Policy") {
    EKS.attachStandardEksWorkerNodeRolePolicies("k8s-installer-test-nodes").unsafeRunSync()
  }
}
