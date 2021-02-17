package com.odenzo.aws.sts

import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint

class SecurityTokenServiceTest extends AWSBaseTest {

  test("Get Caller Identity") {
    val id = SecurityTokenService.getCallerIdentity().unsafeRunSync()
    scribe.info(s"Caller ID ${oprint(id)}")
  }

  test("Get Access Token") {
    val id = SecurityTokenService.getAccessToken().unsafeRunSync()
    scribe.info(s"Token ${oprint(id)}")
  }

}
