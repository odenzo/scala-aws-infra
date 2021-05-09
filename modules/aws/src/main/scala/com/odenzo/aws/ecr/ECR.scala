package com.odenzo.aws.ecr

import cats._
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.utils.{FS2Utils, IOU}
import fs2._
import software.amazon.awssdk.services.ecr.EcrAsyncClient
import software.amazon.awssdk.services.ecr.model._

import scala.jdk.CollectionConverters._
object ECR {

  lazy val client = EcrAsyncClient.create()

  /* There seems to be no way to create or list ECR registry. Maybe one is pre-defined
   * Ours is:
   *   879130378853.dkr.ecr.us-east-1.amazonaws.com
   */

  /** Basic upsert, if repository exists makes sure it is in state, else creates in state */
  def ensureRepository(name: String, scan: Boolean = false, immutable: Boolean = false): IO[Repository] = {
    findRepository(name).flatMap {
      case None       => createRepository(name, scan, immutable)
      case Some(repo) =>
        val isNowImmutable           = repo.imageTagMutabilityAsString() === "IMMUTABLE"
        val isNowScanOnPush: Boolean = repo.imageScanningConfiguration().scanOnPush(): Boolean

        IO(repo) <*
          Monad[IO].unlessA(isNowImmutable === immutable)(setImmutableImages(name, immutable)) <*
          Monad[IO].unlessA(isNowScanOnPush === scan)(setImageScanning(name, scan))

    }
  }

  /** A Simple Request/Response with CompleteableFuture style
    *  Do we turn it into a Stream for consistency?
    */
  def createRepository(name: String, scan: Boolean = false, immutable: Boolean = false): IO[Repository] = {
    val mutability = if (immutable) ImageTagMutability.IMMUTABLE else ImageTagMutability.MUTABLE

    IOU
      .toIO {
        client.createRepository(
          CreateRepositoryRequest.builder
            .repositoryName(name)
            .imageTagMutability(mutability)
            .imageScanningConfiguration(ImageScanningConfiguration.builder.scanOnPush(scan).build())
            .build()
        )
      }
      .map(_.repository)
  }

  def describeRepositories(): IO[Stream[IO, Repository]] = {
    for {
      stream <- FS2Utils.toStream(client.describeRepositoriesPaginator())
      burst   = stream.map(_.repositories().asScala.toList).flatMap(Stream.emits)
    } yield burst
  }

  def findRepository(name: String): IO[Option[Repository]] = {
    describeRepositories().flatMap(stream => stream.filter(_.repositoryName === name).compile.last)
  }

  /** Not using filters on API side */
  def listImages(repo: String) = {
    for {
      stream <- FS2Utils.toStream(client.describeImagesPaginator(DescribeImagesRequest.builder.repositoryName(repo).build()))
      content = stream.map(_.imageDetails().asScala.toList).flatMap(fs2.Stream.emits)
    } yield content
  }

  /** Uses default registry for user account */
  def setImageScanning(repo: String, onPush: Boolean) = {
    IOU.toIO {
      client.putImageScanningConfiguration(
        PutImageScanningConfigurationRequest.builder
          .repositoryName(repo)
          .imageScanningConfiguration(ImageScanningConfiguration.builder.scanOnPush(onPush).build())
          .build()
      )
    }
  }

  /** Uses default registry for user account */
  def setImmutableImages(repo: String, immutableTags: Boolean) = {
    IOU.toIO {
      client.putImageTagMutability(
        PutImageTagMutabilityRequest.builder
          .repositoryName(repo)
          .imageTagMutability(if (immutableTags) "IMMUTABLE" else "MUTABLE")
          .build()
      )
    }
  }

  /** Gets the access token for default ECR registry based on caller creds (access token actually, same as aws ecr get-login-password?
    * This will also return the proxy endpoint yeah!
    */
  def getECRPassword(): IO[AuthorizationData] = {
    IOU.toIO(client.getAuthorizationToken()).map(_.authorizationData().asScala.toList) >>= IOU.exactlyOne(s"ECR Token")
  }

  def getEndpoint(): IO[String] = {
    getECRPassword().map(_.proxyEndpoint())
  }

  def getEndpointHost(): IO[String] = {
    getECRPassword().map(_.proxyEndpoint().drop("https://".length))
  }
}
