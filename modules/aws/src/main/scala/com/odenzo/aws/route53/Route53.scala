package com.odenzo.aws.route53

import cats.effect.IO
import com.odenzo.aws.OTags
import com.odenzo.utils.{FS2Utils, IOU}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.route53.Route53AsyncClient
import software.amazon.awssdk.services.route53.model._
import software.amazon.awssdk.services.route53domains.Route53DomainsAsyncClient
import software.amazon.awssdk.services.route53domains.model.DomainSummary

import scala.jdk.CollectionConverters._

/** COvers Route53 and Route53 domains API usage */
object Route53 {
  // Should make a Typeclass for all these
  val toRoutew53Tag: (String, String) => Tag              = (k: String, v: String) => Tag.builder().key(k).value(v).build()
  private val r53Domainsclient: Route53DomainsAsyncClient = Route53DomainsAsyncClient.builder.region(Region.AWS_GLOBAL).build()
  private val r53Client: Route53AsyncClient               = Route53AsyncClient.builder.region(Region.AWS_GLOBAL).build()

  /** There are no filters on list domains..mmm.. */
  def listDomains(): IO[fs2.Stream[IO, DomainSummary]] = {
    for {
      stream <- FS2Utils.toStream(r53Domainsclient.listDomainsPaginator())
      content = stream.map(r => r.domains().asScala.toList)
      burst   = content.flatMap(fs2.Stream.emits(_))
    } yield burst
  }

  def listHostedZones(): IO[fs2.Stream[IO, HostedZone]] = {
    FS2Utils.toStream(r53Client.listHostedZonesPaginator()).map { stream =>
      stream.map(_.hostedZones().asScala.toList).flatMap(fs2.Stream.emits)
    }
  }

  /** This works and creates right away, indempotent */
  def makeCName(zoneId: String, cnameKey: String, cnameVal: String): IO[ChangeResourceRecordSetsResponse] = {
    val change = Change.builder
      .action(ChangeAction.UPSERT)
      .resourceRecordSet(
        ResourceRecordSet.builder
          .name(cnameKey)
          .`type`(RRType.CNAME)
          .ttl(300L)
          // .region(region.toString)    // This breaks it, AWS rich as hell and they can't document itnerfaces
          .resourceRecords(ResourceRecord.builder.value(cnameVal).build())
          .build()
      )
      .build()

    val rq = ChangeResourceRecordSetsRequest.builder
      .hostedZoneId(zoneId)
      .changeBatch(
        ChangeBatch.builder
          .comment("CNAME Entry for AWS Certification Validation")
          .changes(change)
          .build()
      )
      .build()

    IOU.toIO(r53Client.changeResourceRecordSets(rq))

  }

  def tagRecordSet(id: String, tags: OTags): IO[ChangeTagsForResourceResponse] = {
    IOU.toIO(
      r53Client.changeTagsForResource(
        ChangeTagsForResourceRequest.builder
          .addTags(tags.via(toRoutew53Tag))
          .resourceId(id)
          .build()
      )
    )
  }

  def listRecordSets(zoneId: String): IO[fs2.Stream[IO, ResourceRecordSet]] = {
    val rq = ListResourceRecordSetsRequest.builder.hostedZoneId(zoneId).build()
    FS2Utils.toStream(r53Client.listResourceRecordSetsPaginator(rq)).map { stream =>
      stream.map(_.resourceRecordSets().asScala.toList).flatMap(fs2.Stream.emits)
    }
  }
}
