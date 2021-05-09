package com.odenzo.aws.lambda

import cats.syntax.all._
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.lambda.model.FunctionConfiguration

class LambdaTest extends AWSBaseTest {

  test("List Functions") {
    val res = Lambda.listFunctions().flatMap(_.compile.toList).unsafeRunSync()
    scribe.info(oprint(res))

    // LOL - parTraverse hits AWS rate limiter , No concurrency configs
    val r2 = res
      .map(_.functionName())
      .traverse(Lambda.listConcurrencyConfigs(_).flatMap(_.compile.toList))
      .unsafeRunSync()

    scribe.info(s"CR2 ${oprint(r2)}")

  }

  test("Find Layers") {
    val res = Lambda.listLayers().unsafeRunSync()
    res.foreach { l =>
      scribe.info(s"Layers Found ${oprint(l)}")
    }
  }

  test("Describe Function") {
    val conf: Seq[FunctionConfiguration] = Lambda.findFunction("DevCmsConverter").unsafeRunSync()
    val sorted                           = conf.sortBy(_.version)
    scribe.info(s"Conf ${oprint(sorted)}")
  }

  test("Publish Layer") {
    scribe.debug("PublishLayer")
    val res = Lambda.publishLambdaLayer(s"k8s-ffmpeg-foo", "test-layer", "horn-code", "aws_lambda_layer_ffmpeg-1.0.2.zip").unsafeRunSync()
    scribe.debug(s"Created Layer ${oprint(res)}")
  }
}
