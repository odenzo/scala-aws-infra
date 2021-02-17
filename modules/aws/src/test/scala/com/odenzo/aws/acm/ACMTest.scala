package com.odenzo.aws.acm

import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.acm.model.{CertificateDetail, CertificateStatus, ResourceRecord}

import scala.jdk.CollectionConverters._
class ACMTest extends AWSBaseTest {

  test("List Certs Stream") {
    val prog = for {
      stream <- ACM.listCertificates()
      l      <- stream.compile.toList
    } yield l

    val res = prog.unsafeRunSync()
    scribe.info(s"Res: ${oprint(res)}")
  }

  test("Create ANd Validate?") {
    for {
      created <- ACM.createWildcardCertificate("testing.horn.co")
      info    <- ACM.describeCertificate(created)
    } yield (created, info)
  }

  test("Delete Certificate") {
    val arn = "arn:aws:acm:us-east-1:879130378853:certificate/49cdc8f0-5578-47b1-8d56-4ae2488ff36b"
    ACM.deleteCertificate(arn).unsafeRunSync()
  }

  test("Describe Certificate") {
    val arn = "arn:aws:acm:us-east-1:879130378853:certificate/bf29729c-f871-4375-8e24-85df5ca9ab18"

    val res: CertificateDetail      = ACM.describeCertificate(arn).unsafeRunSync()
    scribe.info(s"Certificate Details: ${oprint(res)}")
    val status: CertificateStatus   = res.status() // CertificateStatus.PENDING_VALIDATION
    val resourceRec: ResourceRecord = res.domainValidationOptions().asScala.head.resourceRecord()
    val certType                    = resourceRec.`type`()
    val dnsName                     = resourceRec.name()
    val dnsValue                    = resourceRec.value()

    scribe.info(s"Route53 CNAME to Make: $dnsName => $dnsValue of type $certType Statys $status")
  }

  test("Describe All Certificates") {
    val res: List[CertificateDetail] = ACM.describeAllCertificates().flatMap(_.compile.toList).unsafeRunSync()
    res.foreach(cd => scribe.debug(s"Certificate: ${oprint(cd)}"))
  }

  test("find for domain") {
    val res = ACM.findTlsCertificateForDomain("eek6.horn.co").unsafeRunSync()
    scribe.debug(s"Res: ${oprint(res)}")
  }
}
