package com.odenzo.aws

import com.odenzo.utils.Secret
import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import software.amazon.awssdk.auth.credentials._

import scala.sys.SystemProperties

case class AWSCredentials(keyId: String, accessKey: Secret, region: String = "us-east-1", accountId: String) {

  def setSystemProperties(): SystemProperties = {
    val props = List(
      "AWS_ACCESS_KEY_ID"     -> keyId,
      "AWS_SECRET_ACCESS_KEY" -> accessKey.secret,
      "AWS_ACCOUNT_ID"        -> accountId //     "879130378853"
    )
    scala.sys.props.addAll(props)
  }
}

object AWSDefault {

  val profileCredentials: AwsCredentials = {
    val provider = DefaultCredentialsProvider.builder().profileName("default").build()
    val creds    = provider.resolveCredentials()
    scribe.info(s"Creds: $creds")
    creds
  }


}

import com.odenzo.utils.Secret
import software.amazon.awssdk.services.ec2.model.CreateKeyPairResponse

/** A summary of created key */
case class AwsPEM(id: String, name: String, material: Secret, fingerPrint: String)

object AwsPEM {
  def from(created: CreateKeyPairResponse): AwsPEM = {
    AwsPEM(created.keyPairId, created.keyName, Secret(created.keyMaterial), created.keyFingerprint)
  }

  def fake(name: String): AwsPEM = {
    AwsPEM("fake-id-exists-already", name, Secret.generate, "hackers-printss")
  }

  implicit val config: Configuration         = Configuration.default
  implicit val codec: Codec.AsObject[AwsPEM] = deriveConfiguredCodec[AwsPEM]
}

case class ServiceSpecificCredentials(id: String, name: String, material: Secret, serviceName: String)
