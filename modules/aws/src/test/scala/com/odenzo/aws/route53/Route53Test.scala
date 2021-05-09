package com.odenzo.aws.route53

import cats.effect._
import cats.effect.syntax.all._

import cats._
import cats.data._
import cats.syntax.all._

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.acm.ACM
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OError
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.route53.model.RRType

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

class Route53Test extends AWSBaseTest {

  test("ListDomains") {
    val res = Route53.listDomains().flatMap(_.compile.toList).unsafeRunSync()
    scribe.info(s"Result: ${oprint(res)}")
  }

  test("ListHostedZones") {
    val res = Route53.listHostedZones().flatMap(_.compile.toList).unsafeRunSync()
    scribe.info(s"Result: ${oprint(res)}")
  }

  test("Examing Existing") {
    val prog = for {
      allZones <- Route53.listHostedZones().flatMap(_.compile.toList)
      zone     <- IO.fromOption(allZones.find(_.name == "horn.co."))(OError(".horn.co. zone not found"))
      str      <- Route53.listRecordSets(zone.id())
      l        <- str.filter(_.`type`() == RRType.CNAME).filter(_.name().startsWith("_")).compile.toList
      _        <- l.traverse(i => IO(scribe.debug(s"CNAME RecSet: ${oprint(i)}")))
    } yield l
    val res  = prog.unsafeRunSync()

    res.foreach { ex =>
      scribe.debug(s"Set Identifier ${ex.setIdentifier()}")
      scribe.debug(s"Traffic PolicY  Identifier ${ex.trafficPolicyInstanceId()}")
    }
  }

  test("CNAME Adding") {
    val certFor = "t5.horn.co"
    val prog    = for {
      allZones   <- Route53.listHostedZones().flatMap(_.compile.toList)
      zone       <- IO.fromOption(allZones.find(_.name == "horn.co."))(OError(".horn.co. zone not found"))
      _          <- IO(scribe.debug(s"Zone: $zone"))
      created    <- ACM.createWildcardCertificate(certFor)
      _          <- IO(scribe.debug(s"Created WildCard Cert $certFor $created"))
      _          <- IO.sleep(FiniteDuration(10, TimeUnit.SECONDS))
      info       <- ACM.describeCertificate(created)
      _          <- IO(scribe.info(s"Info: ${oprint(info)}"))
      resourceRec = info.domainValidationOptions().asScala.head.resourceRecord()
      _          <- Route53.makeCName(zone.id, resourceRec.name, resourceRec.value)
    } yield zone

    prog.unsafeRunSync()
  }
}
