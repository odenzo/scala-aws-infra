package com.odenzo.aws.secretsmanager

import cats.effect.IO
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint

class SecretsManagerTest extends AWSBaseTest {

  test("Listing Secrets") {
    val res = SecretsManager.listSecrets().flatMap(_.compile.toList).unsafeRunSync()
    scribe.info(s"Res: ${oprint(res)}")

  }

  test("Create and GEt") {
    val secretName = "test-12"
    val prog       = for {
      passwd <- SecretsManager.getRandomPassword()
      _      <- IO(scribe.debug(s"Password In: $passwd"))
      arn    <- SecretsManager.createSecret(secretName, passwd, globalTags)
      s1     <- SecretsManager.describeSecret(arn)
      s2     <- SecretsManager.describeSecret(secretName)
      p2     <- SecretsManager.getPassword(arn)
      p3     <- SecretsManager.getPassword(secretName)
      _      <- SecretsManager.deleteSecret(secretName)
    } yield (s1, s2, passwd, p2, p3)

    val res = prog.unsafeRunSync()
    scribe.info(s"Res: ${oprint(res)}")
  }

  // Status Code 480
  test("Deleting Non-Existant") {
    val prog = SecretsManager.deleteSecret("test-foo") *> SecretsManager.deleteSecret("test-too")
    val res  = prog.unsafeRunSync()
    scribe.info(s"Res: ${oprint(res)}")
  }

  // Code 400 ResourceNotFoundException
  test("Get Non-Existant") {
    val res = SecretsManager.getPassword("hahah").unsafeRunSync()
    scribe.info(s"Res: ${oprint(res)}")
  }
}
