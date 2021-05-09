package com.odenzo.aws.secretsmanager

import cats.effect.IO
import com.odenzo.aws.OTags
import com.odenzo.utils.{FS2Utils, IOU}
import software.amazon.awssdk.services.secretsmanager.SecretsManagerAsyncClient
import software.amazon.awssdk.services.secretsmanager.model._

import scala.jdk.CollectionConverters.CollectionHasAsScala

/** Amazon Secrets Manager */
object SecretsManager {
  private val client = SecretsManagerAsyncClient.create()

  val toSecretsManagerTag: (String, String) => Tag = (k: String, v: String) => Tag.builder().key(k).value(v).build()

  def getRandomPassword(): IO[String] =
    IOU
      .toIO(
        client.getRandomPassword(
          GetRandomPasswordRequest.builder
            .excludePunctuation(true)
            .includeSpace(false)
            .passwordLength(20)
            .requireEachIncludedType(true)
            .build()
        )
      )
      .map(_.randomPassword())

  /** Creates a the secret and returns the ARN */
  def createSecret(name: String, secret: String, tags: OTags): IO[String] = {
    IOU
      .toIO(
        client.createSecret(
          CreateSecretRequest.builder
            .name(name)
            .description("Shared between clusters to do symmetric encryption of admin files")
            .secretString(secret)
            .tags(tags.via(toSecretsManagerTag))
            .build()
        )
      )
      .map(_.arn())
  }

  def deleteSecret(nameOrArn: String): IO[DeleteSecretResponse] = {
    IOU.toIO(client.deleteSecret(DeleteSecretRequest.builder().secretId(nameOrArn).build()))
  }

  /** All the secrets metadata, not including the actual secret */
  def listSecrets(): IO[fs2.Stream[IO, SecretListEntry]] = {
    for {
      stream <- FS2Utils.toStream(client.listSecretsPaginator())
      burst   = stream.map(_.secretList().asScala.toList).flatMap(fs2.Stream.emits)
    } yield burst
  }

  /** Throws some error if not found at AWS layer */
  def describeSecret(nameOrArn: String): IO[DescribeSecretResponse] = {
    IOU.toIO(client.describeSecret(DescribeSecretRequest.builder.secretId(nameOrArn).build()))
  }

  /** Gets the password, as a String not binary */
  def getPassword(nameOrArn: String) = {
    IOU
      .toIO(client.getSecretValue(GetSecretValueRequest.builder.secretId(nameOrArn).build()))
      .map(_.secretString())
  }
}
