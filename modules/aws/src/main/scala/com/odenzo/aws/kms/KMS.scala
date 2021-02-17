package com.odenzo.aws.kms

import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import com.odenzo.aws.OTags
import com.odenzo.utils.{FS2Utils, IOU}
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.kms.KmsAsyncClient
import software.amazon.awssdk.services.kms.model._

import scala.jdk.CollectionConverters._

/** KMS = Key Management Service , see also client side
  *  libs: https://docs.aws.amazon.com/encryption-sdk/latest/developer-guide/introduction.html
  *  look ma, no braces style for a try on.
  */
object KMS {
  private val client: KmsAsyncClient = KmsAsyncClient.create()

  val toKmsTag: (String, String) => Tag = (k: String, v: String) => Tag.builder().tagKey(k).tagValue(v).build()

  def listAliases()(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, AliasListEntry]] =
    for {
      // Can only limit by ARN or KeyId
      stream <- FS2Utils.toStream(client.listAliasesPaginator())
      burst   = stream.map(_.aliases().asScala.toList).flatMap(fs2.Stream.emits)
    } yield burst

  def listKeys()(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, KeyListEntry]] =
    for {
      stream <- FS2Utils.toStream(client.listKeysPaginator()) // NoFilters
      burst   = stream.map(_.keys.asScala.toList).flatMap(fs2.Stream.emits)
    } yield burst

  /** ARN or `alias/$aliasName` or key id can be used as id. */
  def describeKey(id: String)(implicit cs: ContextShift[IO]): IO[DescribeKeyResponse] =
    IOU.toIO(client.describeKey(DescribeKeyRequest.builder.keyId(id).build()))

  /** Returns KeyId of alias (if found). */
  def findKeyIdByAlias(alias: String)(implicit cs: ContextShift[IO]): IO[Option[String]] =
    listAliases().flatMap(
      _.filter(_.aliasName === alias)
        .map(_.targetKeyId)
        .compile
        .toList
    ) >>= IOU.optionOne(s"Key Alias $alias")

  def createAlias(alias: String, keyId: String)(implicit cs: ContextShift[IO]): IO[CreateAliasResponse] =
    IOU.toIO(client.createAlias(CreateAliasRequest.builder().aliasName(alias).targetKeyId(keyId).build()))

  /** Note this  uses default Symmetric encrypr/descrypt WITHOUT defining policies
    * You probably want to make an alias of it too.
    */
  def createSymmetricKey(desc: String, tags: OTags)(implicit cs: ContextShift[IO]): IO[CreateKeyResponse] =
    IOU.toIO(
      client.createKey(
        CreateKeyRequest.builder
          .customerMasterKeySpec(CustomerMasterKeySpec.SYMMETRIC_DEFAULT)
          .description(desc)
          .keyUsage(KeyUsageType.ENCRYPT_DECRYPT)
          .origin(OriginType.AWS_KMS)
          .tags(tags.via(toKmsTag))
          .build()
      )
    )

  /** API Based Encryption --- not sure how much is remote. Client side we can use the AWS Crypto lib.
    * Uses default AWS Symmetric
    */
  def encrypt(msg: String, keyId: String, context: Map[String, String])(implicit cs: ContextShift[IO]): IO[EncryptResponse] =
    IOU.toIO(
      client.encrypt(
        EncryptRequest.builder
          .encryptionAlgorithm(EncryptionAlgorithmSpec.SYMMETRIC_DEFAULT)
          .keyId(keyId)
          .grantTokens()
          .encryptionContext(context.asJava)
          .plaintext(SdkBytes.fromUtf8String(msg))
          .build()
      )
    )

  /** Descrypting with AWS default symettrix algo */
  def decrypt(bytes: Array[Byte], keyId: String, context: Map[String, String])(implicit cs: ContextShift[IO]): IO[DecryptResponse] =
    IOU.toIO(
      client.decrypt(
        DecryptRequest.builder
          .encryptionAlgorithm(EncryptionAlgorithmSpec.SYMMETRIC_DEFAULT)
          .keyId(keyId)
          .grantTokens()
          .encryptionContext(context.asJava)
          .ciphertextBlob(SdkBytes.fromByteArray(bytes))
          .build()
      )
    )
}
