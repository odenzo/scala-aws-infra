package com.odenzo.sops

import cats.effect.unsafe.IORuntime
import com.odenzo.utils.OPrint.oprint
import io.circe.Json
import io.circe.literal.JsonStringContext
import munit.FunSuite

import java.io.File
import scala.concurrent.ExecutionContext

class SOPSTest extends FunSuite {
  val ex: ExecutionContext    = scala.concurrent.ExecutionContext.Implicits.global
  implicit val IOR: IORuntime = IORuntime.global
  test("JSON Encrypt to File") {
    val json      = json"""{ "foo": "boo" }"""
    val encFile   = new File("/tmp/here.enc.json")
    val rs        = SOPS.encrypt(json, encFile).unsafeRunSync()
    scribe.info(s"STDOUT: ${oprint(rs)}")
    val roundTrip = SOPS.decryptJson(encFile).unsafeRunSync()
    scribe.info(s"RoundTripped: ${roundTrip.spaces4}")
  }

  test("Decrypting YAML with _unencrypted") {
    val data: Json = SOPS.decryptYaml(new File("data_dir/eek6/in/googleClientConfig.enc.yaml")).unsafeRunSync()
    scribe.info(s"Data ${data.spaces4}")

    val noUnencrypted = SOPS.modifyFieldNamesStrippingUnencrypted(data)
    scribe.info(s"Data Processed ${noUnencrypted.spaces4}")

  }
}
