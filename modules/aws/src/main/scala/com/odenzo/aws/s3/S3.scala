package com.odenzo.aws.s3

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, AwsErrorUtils, OTag, OTags}
import com.odenzo.utils.OPrint.oprint
import com.odenzo.utils.{FS2Utils, IOU, SemanticVersion}
import io.circe.Json
import io.circe.literal.JsonStringContext
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.model._

import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.util.matching.UnanchoredRegex

/** Basic AWS S3 for Bucket and Folders Making. This has very different API
  *
  * S3 Notes: Buckets contains objects. Can grant rights to read/write. I think can pass pre-signed URLs as needed
  * Should read the S3 stuff. What we really want is withwe signed URLs or reads restricted to apps with roles.
  * Not sure how they do currently, but all publicly exposed to read I think.
  */
object S3 extends AWSUtils with AwsErrorUtils {
  lazy val client: S3AsyncClient             = S3AsyncClient.builder.region(Region.US_EAST_1).build
  //http://acs.amazonaws.com/groups/global/AllUsers
  private def s3Tag: (String, String) => Tag = (key: String, v: String) => Tag.builder().key(key).value(v).build()

  /** Returns the location, which is not too exciting. This doesn't fail if bucker already exists beliedve it or not!?
    * Will Tag the bucket too.
    */
  def createBucket(name: String, acl: BucketCannedACL, tags: OTags): IO[String] = {
    IO(scribe.debug(s"Creating S3 Bucket $name ACL $acl")) *>
      IOU
        .toIO(
          client.createBucket(
            CreateBucketRequest.builder
              .bucket(name)
              .acl(acl) // This BUCKET ACL controls listing, the object ACL controlls reading the object
              .build
          )
        )
        .map(_.location)
        .productL(tagBucket(name, tags))
  }

  /** FOr thw common use case, owner owns. Public can read objects but not list contents. Owner rwx
    * No versioning or object locks.
    */
  def createPublicBucket(bucketName: String, tags: OTags): IO[Bucket] = {
    // Accomplished hopefully with just a Policy
    for {
      _       <- createBucket(bucketName, BucketCannedACL.PRIVATE, tags)
      _       <- setBucketPolicyPublicRead(bucketName)
      nbucket <- getBucket(bucketName) // Hopefully not a race
    } yield nbucket
  }

  def setBucketPolicyPublicRead(bucketName: String): IO[Unit] = {
    val statementId = UUID.randomUUID().toString
    val policyId    = UUID.randomUUID().toString
    val resourceArn = s"arn:aws:s3:::$bucketName/*" // Maybe just bucket/generated/*?
    val policy      = json"""{
        "Id": $policyId,
        "Statement": [
          {
            "Sid": $statementId,
            "Action": [  "s3:GetObject"  ],
            "Effect": "Allow",
            "Resource": $resourceArn,
            "Principal": {              "AWS": [  "*"  ]            }
          }
        ]
      }"""
    putPolicy(bucketName, policy)
  }

  /** Error if not existing */
  def getPolicy(bucketName: String): IO[GetBucketPolicyResponse] = {
    IOU.toIO(client.getBucketPolicy(GetBucketPolicyRequest.builder().bucket(bucketName).build()))
  }

  def putPolicy(bucketName: String, policy: Json): IO[Unit] = {
    IOU
      .toIO(
        client.putBucketPolicy(
          PutBucketPolicyRequest.builder
            .bucket(bucketName)
            .confirmRemoveSelfBucketAccess(false)
            .policy(policy.spaces4)
            .build()
        )
      )
      .void
  }

  def getACL(bucketName: String): IO[GetBucketAclResponse] = {
    IOU.toIO(client.getBucketAcl(GetBucketAclRequest.builder().bucket(bucketName).build))
  }

  def tagBucket(bucketName: String, tags: OTags): IO[Unit] = {
    IOU
      .toIO(
        client
          .putBucketTagging(
            PutBucketTaggingRequest.builder
              .tagging(Tagging.builder.tagSet(tags.via(s3Tag)).build)
              .bucket(bucketName)
              .build
          )
      )
      .void
  }

  /** No Pagination, no publisher, no scrolling no filter. Only returns bucket name.  Yeah AWS
    * This scrolls through (sequentially for now) all the buckets and returns each deleted bucket.
    * Note that the delete bucket might have to delete folders and objects other stuff (triggers).
    * Only folders and objects will be added here. Whoever added other stuff needs to delete that.
    */
  def deleteBucketsWithTag(tag: OTag): IO[List[(Bucket, List[Tag])]] = {
    val atag: Tag = S3.s3Tag.tupled(tag.v)
    IOU
      .toIO(client.listBuckets())
      .flatMap { rs =>
        fs2.Stream
          .emits(AWSUtils.fromJList(rs.buckets))
          .covary[IO]
          .debug(oprint(_), logger = scribeInfo)
          .evalMap(b => getBucketTags(b.name()).tupleLeft(b))
          .debug(oprint(_), scribeInfo)
          .filter { case (_, lt) => lt.contains(atag) }
          .evalTap { t => deleteBucketAndObjects(t._1.name()) }
          .compile
          .toList
      }

  }

  def deleteBucketAndObjects(bucketName: String): IO[String] = {
    fs2
      .Stream(bucketName)
      .debug(oprint(_), scribeInfo)
      .evalTap { bn => emptyBucket(bn) }
      .evalTap { bn => deleteBucket(bn) }
      .compile
      .lastOrError
  }

  def getBucketTags(bucketName: String): IO[List[Tag]] = {

    // This  error if no tagSet, no consistency at all...sigh. NoSuchTagSet nested err
    IOU
      .toIO(client.getBucketTagging(GetBucketTaggingRequest.builder().bucket(bucketName).build()))
      .map(rs => if (rs.hasTagSet) fromJList(rs.tagSet) else List.empty)
      .handleErrorWith(awsNestedErrorHandler {
        case e: S3Exception if e.awsErrorDetails.errorCode == "NoSuchTagSet" => IO.pure(List.empty)
      })

  }

  /** We just have to make this up from a pattern. aws.Bucket doesn't have it. */
  def getBucketArn(name: String): IO[String] = IO.pure(s"arn:aws:s3:::$name")

  /** List the buckets owned by the caller/IAM */
  def listBuckets(): IO[List[Bucket]] = IOU.toIO(client.listBuckets()).map(_.buckets().asScala.toList)

  def findBucket(name: String): IO[Option[Bucket]] = listBuckets().map(_.find(_.name === name))

  def getBucket(name: String): IO[Bucket] = findBucket(name) >>= IOU.required(s"Bucket $name")

  def getWebsiteServerConfig(bucketName: String): IO[GetBucketWebsiteResponse] = {
    IOU.toIO(client.getBucketWebsite(GetBucketWebsiteRequest.builder.bucket(bucketName).build()))
  }

  def deleteBucketIfExists(name: String): IO[Unit] = {
    findBucket(name).flatMap {
      case None    => IO(scribe.info(s"S3 Bucket $name didn't exist so no deleting."))
      case Some(_) => deleteBucket(name).void
    }
  }

  /** Probably has to be empty first */
  def deleteBucket(name: String): IO[DeleteBucketResponse] = {
    IOU.toIO(client.deleteBucket(DeleteBucketRequest.builder().bucket(name).build))
  }

  /** Enable versionioning on an S3 Bucket, enabled or suspendeds */
  def versionBucket(name: String, enabled: Boolean): IO[PutBucketVersioningResponse] = {
    IOU.toIO {
      client.putBucketVersioning(
        PutBucketVersioningRequest
          .builder()
          .bucket(name)
          .versioningConfiguration(
            VersioningConfiguration.builder
              .status(if (enabled) BucketVersioningStatus.ENABLED else BucketVersioningStatus.SUSPENDED)
              .build()
          )
          .build()
      )
    }
  }

  /** List the contents of a bukcer as a Stream */
  def listBucketContents(bucketName: String): IO[fs2.Stream[IO, S3Object]] = {
    val req = ListObjectsV2Request.builder.bucket(bucketName).build()

    for {
      _      <- IO(scribe.debug(s"Listing $bucketName contents (paging)"))
      stream <- FS2Utils.toStream(client.listObjectsV2Paginator(req))
      content = stream.map(_.contents().asScala.toList).flatMap(fs2.Stream.emits)
    } yield content
  }

  /** A list of object identifiers, key(s) seem ike they should works
    * Generally chunk the keys into list of 100 or so?
    */
  def deleteObjects(bucketName: String, keys: Seq[String]): IO[DeleteObjectsResponse] = {
    val objIds = keys.map(ObjectIdentifier.builder().key(_).build()).asJavaCollection
    IOU.toIO {
      client.deleteObjects(
        DeleteObjectsRequest.builder
          .bucket(bucketName)
          .delete(Delete.builder.objects(objIds).build())
          .build
      )
    }
    // FIXME:  Meh, this has errors and deleted to check our selves and throw (?)
  }

  /** Stream of responses, each deleting a group of bucket contents.
    * The response has a list of succesful deletions and errors.
    * This just a pass through the errors
    */
  def emptyBucket(bucketName: String): IO[List[S3Error]] = {
    listBucketContents(bucketName).flatMap { stream =>
      stream
        .map(_.key)
        .debug(s => s"Deleting Content $s", scribeInfo)
        .chunkLimit(100)
        .evalMap(chunk => deleteObjects(bucketName, chunk.toList))
        .map(rs => fromJList(rs.errors))
        .debug(s => s"$bucketName trouble ${oprint(s)}", scribeInfo)
        .compile
        .fold(List.empty[S3Error])(_ ::: _)
    }
  }

  private def filterer(folderPrefix: String, endsWith: String): S3KeyFilter =
    S3KeyFilter.builder
      .filterRules(
        FilterRule.builder.name(FilterRuleName.SUFFIX).value(endsWith).build(),
        FilterRule.builder.name(FilterRuleName.PREFIX).value(folderPrefix).build()
      )
      .build()

  /** Creates an Event in S3 bucket for all objects with prefix and suffix filter (e.g. folder/ and .ext).
    * The events gets bound to the existing lambdaFunction as an async trigger
    */
  def addCreatedEventNotification(bucketName: String, prefixFilter: String, suffixFilter: String, lambdaFnArn: String): IO[Unit] = {
    val folderExtFilter = NotificationConfigurationFilter.builder.key(filterer(prefixFilter, suffixFilter)).build()
    // Meh, this used to work, upgrade AWS and get a not-well formed XML or broken schema
    val rq              = PutBucketNotificationConfigurationRequest.builder
      .bucket(bucketName)
      .notificationConfiguration(
        NotificationConfiguration.builder
          .lambdaFunctionConfigurations(
            LambdaFunctionConfiguration.builder
              .id(s"In $prefixFilter with $suffixFilter")
              .lambdaFunctionArn(lambdaFnArn)
              .events(Event.S3_OBJECT_CREATED)
              .filter(folderExtFilter)
              .build()
          )
          .build()
      )
      .build()
    scribe.debug(s"Request: ${oprint(rq)}")
    IOU.toIO(client.putBucketNotificationConfiguration(rq)).void
  }

  /** This is really just overriding the notificationConfiguration to be none (All notifications not just lambda) */
  def deleteBucketEventNotifications(bucketName: String): IO[Unit] = {
    IOU
      .toIO(
        client.putBucketNotificationConfiguration(
          PutBucketNotificationConfigurationRequest.builder
            .bucket(bucketName)
            .notificationConfiguration(NotificationConfiguration.builder.build())
            .build()
        )
      )
      .void
  }

  /** May throw Exceptions and should have a case class or too ;-) */
  def softwareReleasesFilterMap(obj: S3Object): Option[(String, (SemanticVersion, (String, OffsetDateTime)))] = {
    val semanticImage: UnanchoredRegex = """_((\d++).(\d++).(\d++)).(deb|jar|zip)""".r.unanchored
    semanticImage
      .findFirstMatchIn(obj.key())
      .flatMap { m =>
        for {
          version <- SemanticVersion.fromString(m.group(1))
          groupOn  = m.before.toString
          ext      = m.group(5)
        } yield (groupOn, (version, (ext, obj.lastModified.atOffset(ZoneOffset.UTC))))
      }
  }

  /** @param bucket   Bucket Name
    * @param filterMapFn A function which takes the S3 Object and extracts a groupig pattern + anything else.
    *           A None is returned to discard (typically non matching items)
    *           Note the original S3Object may also kept for non-empty, but up to the filterMapFn to do it.
    * @return
    */
  def groupContentsByFilterMap[T, U](bucket: String, filterMapFn: S3Object => Option[(T, U)]): IO[Map[T, List[U]]] = {
    // Note the Map returned contains unsorted lists.
    listBucketContents(bucket)
      .flatMap(stream => stream.mapFilter(filterMapFn).compile.toList)
      .map(l => l.groupMap(_._1)(_._2))
  }

  /** If we can find the bucket then delete the website condig */
  def deleteBucketWebsite(bucketName: String): IO[Unit] = {
    val deleteWebsiteConfig: IO[DeleteBucketWebsiteResponse] =
      IOU.toIO(client.deleteBucketWebsite(DeleteBucketWebsiteRequest.builder.bucket(bucketName).build()))
    for {
      optBucket <- findBucket(bucketName)
      _         <- deleteWebsiteConfig.whenA(optBucket.isDefined)
    } yield ()
  }

  def listObjects(bucketName: String, prefix: String): IO[fs2.Stream[IO, S3Object]] = {
    FS2Utils.toBurstStream {
      client.listObjectsV2Paginator(ListObjectsV2Request.builder().bucket(bucketName).prefix(prefix).build())
    }(c => AWSUtils.fromJList(c.contents()))
  }
}
