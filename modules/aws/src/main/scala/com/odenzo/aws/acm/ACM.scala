package com.odenzo.aws.acm

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, OTags}
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU}
import software.amazon.awssdk.services.acm._
import software.amazon.awssdk.services.acm.model._

import scala.jdk.CollectionConverters._

/** AWS Certificate Manager functions */
object ACM extends AWSUtils {

  //noinspection DuplicatedCode
  val toACMTag: (String, String) => Tag = (k: String, v: String) => Tag.builder.key(k).value(v).build()

  protected final val client = AcmAsyncClient.create()

  def tagCertificate(arn: String, tags: OTags): IO[Unit] = {
    completableFutureToIO(
      client.addTagsToCertificate(
        AddTagsToCertificateRequest.builder
          .certificateArn(arn)
          .tags(tags.via(toACMTag))
          .build()
      )
    ).void
  }

  /** Creates a certificate and asks for DNS validation,
    * @param base Domain like foo.odenzo.com and will create certificate covering foo.horn.co and *.foo.horn.co
    */
  def createWildcardCertificate(base: String): IO[String] = {
    completableFutureToIO(
      client.requestCertificate(
        RequestCertificateRequest.builder
          .domainName(base)
          .subjectAlternativeNames(s"*.$base")
          .validationMethod(ValidationMethod.DNS)
          .idempotencyToken(AWSUtils.idempotentToken)
          .build()
      )
    )
      .map(_.certificateArn())
  }

  def describeCertificates(filters: Filters): IO[fs2.Stream[IO, CertificateSummary]] = {
    for {
      stream <- FS2Utils.toStream(client.listCertificatesPaginator(ListCertificatesRequest.builder.includes(filters).build()))
      emitted = stream.map(rs => fromJList(rs.certificateSummaryList)).flatMap(fs2.Stream.emits)
    } yield emitted
  }

  /** Summary just has arn and domain name. */
  def listCertificates(): IO[fs2.Stream[IO, CertificateSummary]] = {
    describeCertificates(Filters.builder().build())
  }

  def waitForCertificateToBeDefined(arn: String): IO[Unit] = {
    checkUntil(findCertificate(arn))(_.isDefined)
  }

  def findCertificate(arn: String): IO[Option[CertificateDetail]] =
    describeCertificate(arn).redeemWith(error => IO.raiseError(error), bind => { IO.pure(bind.some) })

  /** Error if certificate with given arn is not found. */
  def describeCertificate(arn: String): IO[CertificateDetail] = {
    completableFutureToIO(client.describeCertificate(DescribeCertificateRequest.builder.certificateArn(arn).build()))
      .map(_.certificate())
  }

  def deleteCertificate(arn: String): IO[DeleteCertificateResponse] = {
    completableFutureToIO(client.deleteCertificate(DeleteCertificateRequest.builder.certificateArn(arn).build()))
  }

  /** k8s/clusterName is typical name. This filters via contain */
  def listCertificatesWithDomain(domain: String): IO[fs2.Stream[IO, CertificateSummary]] = {
    listCertificates().map { stream =>
      stream.filter(_.domainName().contains(domain))
    }
  }

  /** Because typically have very few certificates. REST Call cost basically N+1 where N is number of certificates */
  def describeAllCertificates(): IO[fs2.Stream[IO, CertificateDetail]] = {
    listCertificates().map { stream =>
      stream.parEvalMap(5)(summary => describeCertificate(summary.certificateArn))
    }
  }

  def findTlsCertificateForDomain(domain: String): IO[Option[CertificateDetail]] = {

    val res: IO[List[CertificateDetail]] = describeAllCertificates().flatMap { stream =>
      stream
        .filter(_.domainName().equals(domain))
        .filter(_.extendedKeyUsages().asScala.exists(_.name().equals(ExtendedKeyUsageName.TLS_WEB_CLIENT_AUTHENTICATION)))
        .compile
        .toList
    }
    res >>= IOU.optionOne(s"TLS Certificate for $domain")
  }

  /** Has to be primary domain as listed in console -- for us always the x.y.z first, not *.x.y.z */
  def deleteAllCertificatesForDomain(domain: String): IO[Unit] = {
    listCertificatesWithDomain(domain).flatMap { stream =>
      stream
        .evalTap(c => IO(scribe.debug(s"Deleting Certificate ${oprint(c)}")))
        .evalMap(cert => deleteCertificate(cert.certificateArn))
        .compile
        .toList
    }.void
  }
}
