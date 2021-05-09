package com.odenzo.aws.kms

import cats.MonadError
import cats.effect.IO
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint

class KMSTest extends AWSBaseTest {

  test("List Keys") {
    val res = KMS.listKeys().flatMap(_.compile.toList).unsafeRunSync()
    scribe.debug(s"Res; ${oprint(res)}")
  }

  test("Describe All Key") {
    KMS
      .listKeys()
      .flatMap { stream =>
        stream
          .debug()
          .parEvalMap(10)(entry => KMS.describeKey(entry.keyId()))
          .evalTap(dr => IO(scribe.debug(s"\nDescribed: ${oprint(dr)}\n")))
          .compile
          .toList
      }
      .unsafeRunSync()

  }

  test("Describe Key Not There") {
    MonadError
    val prog: IO[Option[String]] = KMS.findKeyIdByAlias("alias/foobar")
    val rs                       = prog.unsafeRunSync()
    scribe.info(s"Res: ${oprint(rs)}")
  }

  test("Create Key and Roundtrip") {
    val prog = for {
      key <- KMS.createSymmetricKey("test-key", globalTags.withName("k8s-test-key")) // This is eventual consistency
      text = "This is some kjson or something"
      enc <- KMS.encrypt(text, key.keyMetadata().keyId(), Map.empty)
      _   <- IO(scribe.debug(s"Enc: ${oprint(enc)}"))
      dec <- KMS.decrypt(enc.ciphertextBlob().asByteArray(), key.keyMetadata().keyId(), Map.empty)
      _   <- IO(scribe.debug(s"Dec: ${oprint(dec)}"))
      txt2 = dec.plaintext().asUtf8String()
    } yield (text, txt2)

    val (txt, t2) = prog.unsafeRunSync()
    assertEquals(txt, t2)

  }

}
