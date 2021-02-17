package com.odenzo.aws.ecr

import cats.effect.IO
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.aws.{OTags, ScribeControl}
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.ecr.model.{AuthorizationData, Repository}

class ECRTest extends AWSBaseTest {
  ScribeControl.sush()
  val stdTags: OTags = OTags.empty.withTag("owner" -> "Steve").withTag("Project" -> "Dev")

  test("List Repositories") {

    val repos = ECR.describeRepositories().flatMap(_.compile.toList).unsafeRunSync()
    scribe.info(s"Repos: ${oprint(repos)}")
  }

  test("Create A New Repository") {
    val prog: IO[Repository] = ECR.createRepository("footest", scan = true, immutable = true)
    val rs                   = prog.unsafeRunSync()
    scribe.info(s"Created Repository: $rs")

  }

  test("Toggle Scanning") {
    val rs  = ECR.setImageScanning("footest", onPush = false).unsafeRunSync()
    val rs2 = ECR.setImageScanning("footest", onPush = true).unsafeRunSync()
    scribe.debug(s"RS: ${oprint(rs)}")
    scribe.debug(s"RS: ${oprint(rs2)}")
  }

  test("Toggle Immutability") {
    val rs  = ECR.setImmutableImages("footest", true).unsafeRunSync()
    val rs2 = ECR.setImmutableImages("footest", false).unsafeRunSync()
    scribe.debug(s"RS: ${oprint(rs)}")
    scribe.debug(s"RS: ${oprint(rs2)}")
  }

  test("Create and Then Re-Create") {
    val c = ECR.createRepository("footest", scan = true, immutable = true).unsafeRunSync()
    val m = ECR.createRepository("footest", scan = false, immutable = true).unsafeRunSync()
    scribe.info(s"Created ${oprint(c)}")
    scribe.info(s"Modified ${oprint(m)}")
  }

  test("Get Access Token") {
    val prog: IO[AuthorizationData] = ECR.getECRPassword()
    val rs                          = prog.unsafeRunSync()
    scribe.info(s"Auth Data: $rs")
  }
}
