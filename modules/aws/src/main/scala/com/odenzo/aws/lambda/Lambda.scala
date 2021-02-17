package com.odenzo.aws.lambda

import cats._
import cats.data._
import cats.effect.{ContextShift, IO}
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, OTag, OTags}
import com.odenzo.utils.{FS2Utils, IOU}
import software.amazon.awssdk.services.lambda.LambdaAsyncClient
import software.amazon.awssdk.services.lambda.model._

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

/** There are so many params to pass, these are just reminders almost.
  * See a sample client, like ImageResizer for more useful examples (and the "tests")
  * WIP Lambda code
  */
object Lambda {
  val client: LambdaAsyncClient = LambdaAsyncClient.create()

  // Lambda Tags are an actual Map[String,String]

  /** Publish a layer, by bucket and key (no object version). Zip File can be used to publish directly from local disk
    * but we go via S3 for repeatability. For FF-MPEG Java only really.
    * This will make a new version of the layer. Publishing it do we want to do? I think yes.
    * Then we can potentially update code / environment / layer reference.
    * Have to be careful the lambda function doesn't just take the latest all the time I guess.
    */
  def publishLambdaLayer(layerName: String, desc: String, bucket: String, key: String)(
      implicit cs: ContextShift[IO]
  ): IO[PublishLayerVersionResponse] =
    IO(scribe.info(s"Publishing new Lambda Layer: $layerName - $key")) *> IOU.toIO {
      client.publishLayerVersion(
        PublishLayerVersionRequest.builder
          .layerName(layerName)
          .compatibleRuntimes(Runtime.JAVA11, Runtime.JAVA8, Runtime.JAVA8_AL2)
          .description(desc)
          .content(
            LayerVersionContentInput.builder
              .s3Bucket(bucket)
              .s3Key(key)
              .build()
          )
          .build()
      )
    }

  def getLayer(arn: String)(implicit cs: ContextShift[IO]): IO[GetLayerVersionByArnResponse] = {
    IOU.toIO(client.getLayerVersionByArn(GetLayerVersionByArnRequest.builder().arn(arn).build()))
  }

  /** @return a list of LayerItems with the most recent versions of each layer */
  def listLayers()(implicit cs: ContextShift[IO]): IO[List[LayersListItem]] = {
    FS2Utils.toList(client.listLayersPaginator().layers())
  }

  /** Get details of the lastest version of a layer by the layer name. Could be an Option I guess?
    * FIXME: Broken Wants Layer Version ARN. But no use now...
    */
  @deprecated("Needs Fixing")
  def listLayerDetailsByName(name: String)(implicit cs: ContextShift[IO]): IO[Option[GetLayerVersionByArnResponse]] = {
    // Note: This gets the lastest "AWS" version version. I guess the idea of a layer is all functions use the same

    FS2Utils
      .toBurstStream(client.listLayersPaginator())(rs => AWSUtils.nullsafeFromList(rs.layers()))
      .flatMap(
        stream =>
          stream
            .filter(_.layerName().equals(name))
            .evalMap(li => getLayer(li.layerArn()))
            .compile
            .toList
      ) >>= IOU.optionOne(s"Lambda Laster $name")
  }

  /** FS2 Stream of versions for a layer */
  def listAllLayerVersions(name: String)(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, LayerVersionsListItem]] = {
    FS2Utils.toBurstStream(client.listLayerVersionsPaginator(ListLayerVersionsRequest.builder().layerName(name).build()))(
      rs => AWSUtils.nullsafeFromList(rs.layerVersions)
    )
  }

//  def removeLayerFromFunction(fnVerionArn:String, layerArn:String)(implicit cs:ContextShift[IO]) = {
//         // client.deleteLayerVersion() will keep a copy until no lambda functions refer to it.
//      client.
//  }

  /** Deletes all the versions of the names layer (if any exist). Deletes the layer doesn't dissaociate */
  def deleteLayerIffExists(layerName: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      _            <- IO(scribe.debug(s"Deleting Layer if Exists [$layerName]"))
      layersStream <- listAllLayerVersions(layerName)
      _            <- layersStream.evalTap(v => Lambda.deleteLayerVersion(layerName, v.version)).compile.drain
    } yield ()
  }

  /** Layer or whatedver */
  def putResourceTags(arn: String, tags: OTags)(implicit cs: ContextShift[IO]): IO[TagResourceResponse] = {
    IOU.toIO(client.tagResource(TagResourceRequest.builder.resource(arn).tags(tags.tags.asJava).build()))
  }

  def getResourceTags(arn: String)(implicit cs: ContextShift[IO]): IO[OTags] = {
    IOU
      .toIO(client.listTags(ListTagsRequest.builder().resource(arn).build()))
      .map(rs => if (rs.hasTags) OTags(rs.tags().asScala.toMap) else OTags.empty)
  }

  def findLayersByNameAndTag(name: String, tag: OTag)(implicit cs: ContextShift[IO]): IO[List[(LayersListItem, OTags)]] = {
    for {
      all      <- listLayers()
      named     = all.filter(l => l.layerName == name)
      withTags <- named.traverse(layer => getResourceTags(layer.layerArn).tupleLeft(layer))
      matching  = withTags.filter { case (_, tags) => tags.contains(tag) }
    } yield matching
  }

  def findLayersByTag(tag: OTag)(implicit cs: ContextShift[IO]): IO[List[(LayersListItem, OTags)]] = {
    for {
      all      <- listLayers()
      withTags <- all.traverse(layer => getResourceTags(layer.layerArn).tupleLeft(layer))
      matching  = withTags.filter { case (_, tags) => tags.contains(tag) }
    } yield matching
  }

  def deleteLayerVersion(layer: String, verNum: Long)(implicit cs: ContextShift[IO]): IO[Unit] =
    IOU.toIO(client.deleteLayerVersion(DeleteLayerVersionRequest.builder.layerName(layer).versionNumber(verNum).build)).void

  def deleteFunctionIfExists(nameOrArn: String, version: Option[String])(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      found <- findFunction(nameOrArn, version)
      _     <- IO(scribe.info(s"Found ${found.size} functions matching $nameOrArn version $version to delete"))
      _     <- if (found.isEmpty) IO.unit else deleteFunction(nameOrArn, version)
    } yield ()
  }

  def deleteFunction(nameOrArn: String, version: Option[String])(implicit cs: ContextShift[IO]): IO[Unit] = {
    val base                              = DeleteFunctionRequest.builder.functionName(nameOrArn)
    val rq: DeleteFunctionRequest.Builder = version.fold(base)(v => base.qualifier(v))
    IOU.toIO(client.deleteFunction(rq.build())).void
  }

  def createFunction(rq: CreateFunctionRequest)(implicit cs: ContextShift[IO]): IO[CreateFunctionResponse] =
    IOU.toIO(client.createFunction(rq))

  /** Config includes the environment and some other stuff. */
  def updateFunctionConfig(
      rq: UpdateFunctionConfigurationRequest
  )(implicit cs: ContextShift[IO]): IO[UpdateFunctionConfigurationResponse] = {
    IOU.toIO(client.updateFunctionConfiguration(rq))
  }
  def updateFunctionCode(rq: UpdateFunctionCodeRequest)(implicit cs: ContextShift[IO]): IO[UpdateFunctionCodeResponse] = {
    IOU.toIO(client.updateFunctionCode(rq))
  }

  def createVpcConfigForLamba(securityGroupIds: List[String], subnetIds: List[String]): VpcConfig =
    VpcConfig.builder
      .securityGroupIds(securityGroupIds.asJavaCollection)
      .subnetIds(subnetIds.asJavaCollection)
      .build()

  /** Not sure why this is so slow other than it is getting every version as an emit */
  def listFunctions()(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, FunctionConfiguration]] = {
    FS2Utils.toBurstStream(
      client.listFunctionsPaginator(
        ListFunctionsRequest.builder
          .functionVersion(FunctionVersion.ALL)
          .build()
      )
    )(t => t.functions().asScala)
  }

  /** Finds function by name, if version suppled filters on that too (slowly) Not could have multople versions found
    * if version not specified
    */
  def findFunction(fn: String, version: Option[String] = None)(implicit cs: ContextShift[IO]): IO[List[FunctionConfiguration]] = {
    listFunctions().flatMap { stream: fs2.Stream[IO, FunctionConfiguration] =>
      stream
        .filter(_.functionName() === fn)
        .filter(fc => version.isEmpty || version.contains(fc.version()))
        .compile
        .toList
    }
  }

  def findLatestFunction(fn: String)(implicit cs: ContextShift[IO]): IO[Option[FunctionConfiguration]] = {
    findFunction(fn, "$LATEST".some) >>= IOU.optionOne(s"$fn::LATEST")
  }

  def listConcurrencyConfigs(fn: String)(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, ProvisionedConcurrencyConfigListItem]] = {
    FS2Utils.toBurstStream(
      client.listProvisionedConcurrencyConfigsPaginator(
        ListProvisionedConcurrencyConfigsRequest.builder
          .functionName(fn)
          .build()
      )
    )(t => t.provisionedConcurrencyConfigs().asScala)
  }

  /** Creating an alias can re-home to different Lambda Fn version, or even split between versions */
  def createAlias(rq: CreateAliasRequest)(implicit cs: ContextShift[IO]): IO[CreateAliasResponse] = {
    IOU.toIO(client.createAlias(rq))
  }

  /** Not sure how to do this, as update alias just updates KMS key for an alias
    * do we have to delete and create? That seems wacked.
    */
  def updateAlias(rq: UpdateAliasRequest)(implicit cs: ContextShift[IO]): IO[UpdateAliasResponse] = {
    IOU.toIO(client.updateAlias(rq))
  }

  def getAlias(aliasName: String)(implicit cs: ContextShift[IO]): IO[GetAliasResponse] = {
    IOU.toIO(
      client.getAlias(
        GetAliasRequest.builder
          //.functionName("foo")
          .name(aliasName)
          .build()
      )
    )
  }

  def updateAlias(aliasName: String, version: String)(implicit cs: ContextShift[IO]): IO[UpdateAliasResponse] = {

    // Going to get the current alias to check its revision and add 1
    // Also, if missing values get erased then populate from old alias
    this.getAlias(aliasName).flatMap { _ =>
      IOU.toIO(
        client.updateAlias(
          UpdateAliasRequest.builder
            .functionVersion(version)
            .revisionId(Instant.now().toString)
            .build()
        )
      )
    }
  }
  def listAliases(fn: String)(implicit cs: ContextShift[IO]): IO[fs2.Stream[IO, AliasConfiguration]] = {
    FS2Utils.toBurstStream(client.listAliasesPaginator(ListAliasesRequest.builder.functionName(fn).build()))(_.aliases().asScala)
  }

  def updateConfiguration(rq: UpdateFunctionConfigurationRequest)(implicit cs: ContextShift[IO]): IO[UpdateFunctionConfigurationResponse] =
    IOU.toIO(client.updateFunctionConfiguration(rq))

  /** https://console.aws.amazon.com/lambda/home?region=us-east-1#/functions/CmsConverter?tab=permissions
    * We actually want to apply this to the BASE function
    */
  def addS3LambdaResourcePermission(fnName: String, version: String, bucketArn: String)(
      implicit cs: ContextShift[IO]
  ): IO[AddPermissionResponse] = {
    IO(scribe.debug(s"Adding Reource Permissions for $fnName Version $version -> $bucketArn")) *>
      IOU.toIO {
        client.addPermission(
          AddPermissionRequest.builder
            .statementId("s3-account") // Aka Sid
            .principal("s3.amazonaws.com")
            .action("lambda:InvokeFunction")
            .functionName(fnName)      // Converted to ARN under Resource: fiueld
            //  .qualifier(version)        // To add to a published version
            .sourceArn(bucketArn)
            .build()
        )
      }
  }

  def addApiGatewayLambdaResourcePermission(fnName: String, version: String, sourceArn: String)(
      implicit cs: ContextShift[IO]
  ): IO[AddPermissionResponse] = {
    IO(scribe.debug(s"Adding APIGateway Lambda Permission $fnName  -> $sourceArn")) *>
      IOU.toIO {
        client.addPermission(
          AddPermissionRequest.builder
            .statementId(UUID.randomUUID().toString) // Aka Sid
            .principal("apigateway.amazonaws.com")
            .action("lambda:InvokeFunction")
            .functionName(fnName)                    // Lots of option, ARN or name
            //  .qualifier(version)                 // To add to a published version
            .sourceArn(sourceArn)                    // ARN of the APIGateway, e.g.
            // "arn:aws:execute-api:us-east-1:879130378853:hw36giybt4/*/*/k8s-ImageResizer-dev"
            .build()
        )
      }
  }

  def addSNSLambdaResourcePermission(fnName: String, sourceArn: String)(
    implicit cs: ContextShift[IO]
  ): IO[AddPermissionResponse] = {
    IO(scribe.debug(s"Adding SNS Lambda Permission $fnName  -> $sourceArn")) *>
      IOU.toIO {
        client.addPermission(
          AddPermissionRequest.builder
            .statementId("sns-account") // Aka Sid
            .principal("sns.amazonaws.com")
            .action("lambda:InvokeFunction")
            .functionName(fnName)      // Converted to ARN under Resource: field
            .sourceArn(sourceArn)
            .build()
        )
      }
  }

  /** Publishes iff changeds to config or function. Seems AWS takes care of version name
    * Please put the underlying code semantic version in description
    */
  def publish(fnName: String, desc: String)(implicit cs: ContextShift[IO]): IO[PublishVersionResponse] = {
    IOU.toIO(
      client.publishVersion(
        PublishVersionRequest
          .builder()
          .functionName(fnName)
          .description(desc)
          // .codeSha256()  // Only if code hash matches this (optional)
          // .revisionId(revision) // Only if revision matching this (optional)  Publish Latest or all?
          .build()
      )
    )
  }

  /** Note: Can't delete function that is bound to an alias */
  def createAlias(alias: String, fnName: String, fnVersion: String, codeVersion: String)(
      implicit cs: ContextShift[IO]
  ): IO[CreateAliasResponse] = {
    val rq = CreateAliasRequest
      .builder()
      .description(s"Image Resizer Alias for Code $codeVersion")
      .functionName(fnName)
      .functionVersion(fnVersion)
      .name(alias)
      //.routingConfig(AliasRoutingConfiguration.builder().additionalVersionWeights())
      .build()

    Lambda.createAlias(rq)
  }
}
