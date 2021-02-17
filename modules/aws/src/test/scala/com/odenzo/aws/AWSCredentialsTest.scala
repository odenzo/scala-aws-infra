package com.odenzo.aws

import com.odenzo.aws.testutils.AWSBaseTest

class AWSCredentialsTest extends AWSBaseTest {

  test("Dump") {
    def aws_env(): Unit = {
      val awsEnv: Map[String, String] = scala.sys.env.view.filterKeys(_.startsWith("AWS")).toMap
      awsEnv.foreachEntry((k, v) => scribe.info(s"Key [$k] -> [$v]"))
    }

    def aws_jenv(): Unit = {
      import scala.jdk.CollectionConverters._
      val awsEnv: Map[String, String] = System.getenv().asScala.view.filterKeys(_.startsWith("AWS")).toMap
      awsEnv.foreachEntry((k, v) => scribe.info(s"Key [$k] -> [$v]"))
    }

    aws_env()
    aws_jenv()

  }

  test("Credentials Picked Up") {
    val creds = AWSDefault.profileCredentials
    scribe.info("Secret Key: " + creds.secretAccessKey().take(5))
  }

}
