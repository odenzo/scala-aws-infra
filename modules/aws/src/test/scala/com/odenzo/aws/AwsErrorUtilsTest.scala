package com.odenzo.aws

import com.odenzo.aws.testutils.AWSBaseTest

class AwsErrorUtilsTest extends AWSBaseTest {

  test("Erasure Good") {
    val err = new IllegalArgumentException("Bad Arg", new IllegalStateException("Missisipi"))

    val res: Option[Nothing] = AwsErrorUtils.nestedRecoverToOption[IllegalStateException](err)
    scribe.debug(s"Good Case OK $res")

  }

  test("Erasure No Cause") {
    val err = new IllegalArgumentException("Bad Arg")
    an[IllegalArgumentException] shouldBe thrownBy(AwsErrorUtils.nestedRecoverToOption[IllegalStateException](err))
  }

  test("Erasure No SubMatch") {
    val err = new IllegalArgumentException("Bad Arg", new IllegalStateException("IL"))
    an[IllegalArgumentException] shouldBe thrownBy(AwsErrorUtils.nestedRecoverToOption[IllegalAccessError](err))
  }
}
