package com.odenzo.utils

class SecretTest extends BaseTest {

  test("Secret Password Generator") {
    val pswd = Secret.generate
    scribe.info("Password: " + pswd.secret)
    scribe.info("Secret: " + pswd)
  }
}
