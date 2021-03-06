package com.odenzo.aws.apigateway

import cats.effect._
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, AwsErrorUtils, OTags}
import com.odenzo.utils.IOU
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.apigatewayv2.ApiGatewayV2AsyncClient
import software.amazon.awssdk.services.apigatewayv2.model._
import software.amazon.awssdk.services.iam.model.Role

import scala.annotation.nowarn
import scala.jdk.CollectionConverters._

object APIGateway extends AWSUtils with AwsErrorUtils {
  lazy private val client = ApiGatewayV2AsyncClient.create()

  /** A thing to make an APIGateway to a lambda function - lots hard coded */
  @nowarn("msg=possible missing interpolator")
  def createAPIGateway(
      name: String,
      desc: String,
      route: String,
      lambdaFnArn: String,
      lambdaRole: Role,
      otags: OTags
  ): IO[CreateApiResponse] =
    IOU
      .toIO {
        client.createApi(
          CreateApiRequest.builder
            .name(name)
            //.credentialsArn(lambdaRole.arn())
            .description(desc)
            .protocolType(ProtocolType.HTTP)
            .disableExecuteApiEndpoint(false)
            .routeKey(route)
            .apiKeySelectionExpression("$request.header.x-api-key")
            .routeSelectionExpression("${request.method} ${request.path}") // NOT EXPANDED, NO INTERPOLATOR INTENDED
            .target(lambdaFnArn)
            .tags(otags.tags.asJava)
            .build()
        )
      }

  /** Creates HTTP_PROXY integration - Binds API Gateway to Lambda Function */
  def createIntegration(apiId: String, lambdaFnArn: String): IO[CreateIntegrationResponse] =
    IOU.toIO {
      client.createIntegration(
        CreateIntegrationRequest.builder
          .apiId(apiId)
          .connectionType(ConnectionType.INTERNET)
          .integrationType(IntegrationType.AWS_PROXY)
          .integrationMethod("POST")
          .integrationUri(lambdaFnArn)
          .payloadFormatVersion("1.0")
          .timeoutInMillis(29000)
          // No Tags
          .build()
      )
    }

  def createRoute(apiId: String, routeKey: String): IO[CreateRouteResponse] =
    IOU.toIO(
      client.createRoute(
        CreateRouteRequest.builder
          .apiId(apiId)
          .apiKeyRequired(false)
          .routeKey(routeKey)
          // .target()   // Target is lambda function?
          .build()
      )
    )

  /** Doeasn't deal with paging. Id is what the id not name */
  def getApiIntegrations(apiId: String): IO[List[Integration]] = {
    IOU.toIO(client.getIntegrations(GetIntegrationsRequest.builder().apiId(apiId).build())).map(_.items().asScala.toList)
  }

  /** Doesn't deal with paging. */
  def getApiRoutes(apiId: String): IO[List[Route]] = {
    IOU.toIO(client.getRoutes(GetRoutesRequest.builder().apiId(apiId).build())).map(_.items().asScala.toList)

    // Not sure the difference, maybe singular get the "short cut" builder ingebration/
    //  client.getIntegration(GetIntegrationRequest.builder().apiId(apiId).build())
  }

  // Seems ApiID is not name. getApis has no filtering...
  def getApiGateways(): IO[List[Api]] =
    completableFutureToIO(client.getApis(GetApisRequest.builder.maxResults("200").build()))
      .map(_.items().asScala.toList)

  def getApiGateway(name: String): IO[List[Api]] =
    getApiGateways().map(_.filter(_.name() === name))

  def getStages(apiId: String): IO[List[Stage]] = {
    //client.getApiMapping(GetApiMappingRequest.builder().apiMappingId("x").domainName("y"))
    completableFutureToIO(client.getStages(GetStagesRequest.builder().apiId(apiId).maxResults("100").build()))
      .map(r => fromJList(r.items()))
  }

  def getIntegrationResponses(apiId: String, integrationId: String): IO[List[IntegrationResponse]] = {
    completableFutureToIO(
      client.getIntegrationResponses(
        GetIntegrationResponsesRequest
          .builder()
          .apiId(apiId)
          .integrationId(integrationId)
          .maxResults("100")
          .build()
      )
    )
      .map(_.items().asScala.toList)
  }

  /** No Scrolling of results, maxes at 100 */
  def getDeployments(apiId: String): IO[List[Deployment]] = {
    completableFutureToIO {
      client.getDeployments(GetDeploymentsRequest.builder().apiId(apiId).maxResults("100").build())
    }.map(r => fromJList(r.items))
  }
  def deleteApiGatewaysByName(name: String): IO[Unit] = {
    for {
      _       <- IO(scribe.debug(s"Deleting API Gateways Names $name"))
      allApis <- getApiGateways()
      apis     = allApis.filter(_.name().equals(name)).toList
      _       <- IO(scribe.debug(s"Found ${oprint(apis)}"))
      _       <- apis.traverse(v => deleteApiGateway(v.apiId))
    } yield ()
  }

  /** ID is different than name. */
  def deleteApiGateway(id: String): IO[DeleteApiResponse] = {
    completableFutureToIO(client.deleteApi(DeleteApiRequest.builder().apiId(id).build()))
  }

}
