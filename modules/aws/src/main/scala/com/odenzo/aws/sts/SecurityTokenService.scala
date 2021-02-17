package com.odenzo.aws.sts

import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import com.odenzo.utils.IOU
import software.amazon.awssdk.services.sts._
import software.amazon.awssdk.services.sts.model._

import scala.jdk.CollectionConverters._

object SecurityTokenService {
  val client: StsAsyncClient = StsAsyncClient.create()

  /** Access token which I think can be for bearer token to call using Kubernetes API? */
  def getAccessToken()(implicit cs: ContextShift[IO]): IO[Credentials] = {
    IOU
      .toIO(client.getSessionToken(GetSessionTokenRequest.builder().durationSeconds(15 * 60).build()))
      .map(_.credentials())
  }

  def assumeRole(roleArn: String, policyArns: List[String])(implicit cs: ContextShift[IO]): IO[AssumeRoleResponse] = {
    IOU.toIO(
      client.assumeRole(
        AssumeRoleRequest.builder
          .roleSessionName("cluster-admintool")
          .roleArn(roleArn)
          .policyArns(policyArns.map(arn => PolicyDescriptorType.builder.arn(arn).build()).asJavaCollection)
          .durationSeconds(15 * 60)
          .build()
      )
    )
  }

  def getCallerIdentity()(implicit cs: ContextShift[IO]) = {
    IOU.toIO(client.getCallerIdentity())
  }

  def getClusterAccessToken(clustr: String)(implicit cs: ContextShift[IO]) = {
    scribe.debug(s"Still Playing with getting *Cluster* access token for $clustr")
    IOU.toIO(client.getSessionToken(GetSessionTokenRequest.builder().durationSeconds(15 * 60).build()))
  }

}
