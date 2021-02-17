package com.odenzo.aws.sns

import cats.effect._
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, OTags}
import com.odenzo.utils.{FS2Utils, IOU}
import software.amazon.awssdk.services.sns._
import software.amazon.awssdk.services.sns.model._

import scala.jdk.CollectionConverters._

/** AWS Simple Notification Service -- pubsub queue */
object SNS extends AWSUtils {

  lazy val client                       = SnsAsyncClient.create()
  //noinspection DuplicatedCode
  val toSNSTag: (String, String) => Tag = (k: String, v: String) => Tag.builder().key(k).value(v).build()

  /** Gets existing topic or creates new one, returns the ARN either way */
  def createTopicIfNotExists(name: String, displayName: Option[String], tags: OTags)(implicit cs: ContextShift[IO]): IO[String] = {
    findTopicNamed(name) >>= IOU.whenEmpty(createTopic(name, displayName, tags))
  }

  /** @return Topic ARN */
  def createTopic(name: String, displayName: Option[String] = None, tags: OTags)(implicit cs: ContextShift[IO]): IO[String] = {
    IOU
      .toIO {
        client.createTopic(
          CreateTopicRequest.builder
            .name(name)
            .tags(tags.via(toSNSTag))
            // Display Name is for SMS and not set
            .attributes(Map("DisplayName" -> displayName.getOrElse(name.take(10))).asJava) // SMS Topic, Retry Polic Etc, Pending
            .build()
        )
      }
      .map(_.topicArn())
  }

  /** Crude email subscribe, skipping the attributes. Note multiple subscriptions for one email are fine, keeps one subscription
    * will notify on each redundant add
    */
  def addEmailSubscription(topicArn: String, email: String)(implicit cs: ContextShift[IO]): IO[Unit] =
    IOU.toIO(client.subscribe(SubscribeRequest.builder.topicArn(topicArn).protocol("email").endpoint(email).build())).void

  def addLambdaFnSubscription(topicArn: String, lambdaFnArn: String)(implicit cs: ContextShift[IO]) =
    IOU.toIO(client.subscribe(SubscribeRequest.builder.topicArn(topicArn).protocol("lambda").endpoint(lambdaFnArn).build())).void

  def listSubcriptions(topicArn: String)(implicit cs: ContextShift[IO]): IO[List[Subscription]] = {
    FS2Utils.toBurstList(client.listSubscriptionsByTopicPaginator(ListSubscriptionsByTopicRequest.builder.topicArn(topicArn).build()))(
      _.subscriptions().asScala.toList
    )
  }

  def deleteTopicByArn(arn: String)(implicit cs: ContextShift[IO]) = {
    IOU.toIO(client.deleteTopic(DeleteTopicRequest.builder().topicArn(arn).build))
  }

  def getTagsForResource(arn: String)(implicit cs: ContextShift[IO]): IO[List[Tag]] = {
    IOU
      .toIO(client.listTagsForResource(ListTagsForResourceRequest.builder().resourceArn(arn).build))
      .flatTap(_ => IO(scribe.info(s"Getting Tags for $arn")))
      .map(_.tags().asScala.toList)
  }

  def getTopicAttributes(arn: String)(implicit cs: ContextShift[IO]): IO[Map[String, String]] = {
    IOU
      .toIO(client.getTopicAttributes(GetTopicAttributesRequest.builder().topicArn(arn).build))
      .flatTap(_ => IO(scribe.info(s"Getting Topic Attr for $arn")))
      .map(_.attributes().asScala.toMap)
  }

  private def listTopics()(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, Topic]] =
    FS2Utils.toStream(client.listTopicsPaginator().topics())

  /** Finds topic by applying filter, typically filter making another call */
  def findTopics[A](fn: Topic => IO[Option[A]])(implicit cs: ContextShift[IO]) = {
    // parEvalMapFilter
    listTopics().flatMap { str => str.evalMapFilter(fn).compile.toList }
  }

  /** Finds topic by applying filter, typically filter making another call */
  def findFirstTopic[A](fn: Topic => IO[Option[A]])(implicit cs: ContextShift[IO]) = {
    listTopics().flatMap { str => str.evalMapFilter(fn).head.compile.last }
  }

  /** @return ARN of topic if found */
  def findTopicNamed(name: String)(implicit cs: ContextShift[IO]): IO[Option[String]] = {
    // Not sure if can have duplicate names
    val matches: IO[List[String]] = listTopicArns().map(_.map(_.topicArn).filter(_.split(':').last.equals(name)))
    matches >>= IOU.optionOne(s"Topic Arn Suffix $name")
  }

  /** Topic only has arn attribe  but the last but is the "Name" */
  def listTopicArns()(implicit cs: ContextShift[IO]): IO[List[Topic]] = listTopics().flatMap(_.compile.toList)

}
