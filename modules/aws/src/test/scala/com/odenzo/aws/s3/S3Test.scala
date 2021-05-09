package com.odenzo.aws.s3

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.SemanticVersion
import software.amazon.awssdk.services.s3.model.{Bucket, BucketCannedACL, S3Object}

class S3Test extends AWSBaseTest {

  test("Bucket Public with Tags") {
    val bn   = "horn-images-installer-test"
    val prog = S3
      .findBucket(bn)
      .flatMap(existing => S3.deleteBucketAndObjects(bn).whenA(existing.isDefined))
      .flatMap(_ => S3.createBucket(bn, BucketCannedACL.PRIVATE, globalTags))
    val res  = prog.unsafeRunSync()
    scribe.info(s"Result Location: $res")
    val acls = S3.getACL(bn).unsafeRunSync()

    scribe.info(s"ACL   ${oprint(acls.grants())}")
    scribe.info(s"Policy ${oprint(S3.getPolicy(bn).unsafeRunSync())}")
  }

  test("Show ACLS") {
    val bn   = "horn-images-installer-test"
    val acls = S3.getACL(bn).unsafeRunSync()
    scribe.info(s"ACL   ${oprint(acls.grants())}")
  }
  test("GET Website Config for Images ") {
    val prog = for {
      foo  <- S3.getWebsiteServerConfig("odenzo-images")
      _    <- IO(scribe.debug(s"HORN: ${oprint(foo)}"))
      conf <- S3.getWebsiteServerConfig("horn-images-dev")
      _    <- IO(scribe.debug(s"Dev: ${oprint(conf)}"))
    } yield (foo, conf)

    prog.unsafeRunSync()

  }

  test("Bucket Private with Tags") {
    val res = S3.createBucket("horn-recordings-installer-test", BucketCannedACL.PRIVATE, globalTags).unsafeRunSync()
    scribe.info(s"Result Location: $res")
  }

  test("List Buckets") {
    val res = S3.listBuckets().unsafeRunSync()
    scribe.info(s"Res $res")
  }

  test("Bucket Context") {
    val prog                = for {
      stream   <- S3.listBucketContents("horn-code")
      contents <- stream.compile.toList
    } yield contents
    val res: List[S3Object] = prog.unsafeRunSync()
    val debs                = res.filter(_.key().endsWith(".deb"))

    debs.foreach { o =>
      val key      = o.key()
      scribe.info(s"Key: ${key}")
      val matchOpt = SemanticVersion.pattern.findFirstMatchIn(key)
      scribe.info(s"Match $matchOpt   Before: ${matchOpt.map(_.before)}")

    }

    debs.groupMap(s3o => SemanticVersion.pattern.findFirstMatchIn(s3o.key).map(_.before).getOrElse("Missing"))(identity)

  }

  test("Delete all Bucket Notification") {
    val res: Unit = S3.deleteBucketEventNotifications("horn-recordings-eek6").unsafeRunSync()
    scribe.debug(oprint(res))
  }

  test("Bucket Buster") {

    val mapFn = S3.softwareReleasesFilterMap _
    val ans   = S3.groupContentsByFilterMap("horn-code", mapFn).unsafeRunSync()
    ans.keys.toList
      .filterNot(_.contains("development"))
      .filterNot(_.startsWith("aws_lambda"))
      .sorted
      .foreach { artifact =>
        val versions  = ans.getOrElse(artifact, List.empty)
        val sversions = versions.sortBy(_._1).takeRight(3)
        scribe.debug(s"\nArtifact: $artifact " + sversions.mkString("\n\t", "\n\t", "\n\n"))
      }
  }

  // Hmm....
  test("Add Created File Event to Lambda") {
    S3
      .addCreatedEventNotification(
        "horn-recordings-eek6",
        "queue/",
        ".cms",
        "arn:aws:lambda:us-east-1:879130378853:function:k8s-eek6-CmsConverter"
      )
      .unsafeRunSync()
  }

  test(" deleteBucketWithObjects()") {
    val prog = List("horn-images-dev", "horn-images-installer-test")
      .traverse { bn: String =>
        S3.findBucket(bn).flatMap { existing: Option[Bucket] =>
          IO(scribe.debug(s"Bucket $bn  Exists: ${existing.isDefined}")) *>
            S3.deleteBucketAndObjects(bn).whenA(existing.isDefined)
        }
      }
    val res  = prog.unsafeRunSync()
    scribe.info(oprint(res))
  }
}
