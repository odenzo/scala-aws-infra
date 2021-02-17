package com.odenzo.aws.rds

import com.odenzo.aws.testutils.AWSBaseTest

class AuroraPostgresTest extends AWSBaseTest {
  test("Deletion of Aurora") {
    val res = AuroraPostgres.teardown("manual-test").unsafeRunSync()
    scribe.info(s"Res: $res")
  }

}
