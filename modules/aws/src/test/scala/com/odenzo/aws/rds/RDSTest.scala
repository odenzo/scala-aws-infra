package com.odenzo.aws.rds

import com.odenzo.aws.testutils.AWSBaseTest

class RDSTest extends AWSBaseTest {

  test("Getting Instances vs Clusters") {
    RDS.describeClusters()
  }
}
