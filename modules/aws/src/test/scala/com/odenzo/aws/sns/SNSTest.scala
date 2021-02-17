package com.odenzo.aws.sns

import cats.effect.IO
import cats.implicits.catsSyntaxFlatMapOps
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.IOU
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.sns.model.Topic

class SNSTest extends AWSBaseTest {

  test("List Topics") {
    val res: List[Topic] = SNS.listTopicArns().unsafeRunSync()
    scribe.debug(s"ARNS: ${oprint(res)}")
  }

  test("Dump All Existing") {
    val name = "DevLambdaFailuresTopic"
    dumpAll(name)
  }

  test("Create/Dump/Delete") {
    val name = "k8s-installer-test"
    val prog = for {
      topicArn <- SNS.findTopicNamed(name) >>= IOU.whenEmpty(SNS.createTopic(name, None, globalTags))
      _        <- SNS.addEmailSubscription(topicArn, "stevef@odenzo.com")
      _        <- dumpAll(name)
//      _        <- SNS.deleteTopic(topicArn)
    } yield ()
    prog.unsafeRunSync()
  }

  def dumpAll(name: String) = {
    for {
      topicArn <- SNS.findTopicNamed(name) >>= IOU.required(s"Topic Named $name")
      _        <- IO(scribe.debug(s"ARNS: ${oprint(topicArn)}"))
      tags     <- SNS.getTagsForResource(topicArn)
      attr     <- SNS.getTopicAttributes(topicArn)
      subs     <- SNS.listSubcriptions(topicArn)
      _        <- IO(scribe.debug(s"Subs: ${oprint(subs)}"))
      _        <- IO(scribe.debug(s"Tags: ${oprint(tags)}"))
      _        <- IO(scribe.debug(s"Attr: ${oprint(attr)}"))
    } yield ()
  }

}
