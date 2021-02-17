package com.odenzo.aws.apigateway

import cats._
import cats.data._
import cats.effect._
import cats.effect.syntax.all._
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, AwsErrorUtils, OTags}
import com.odenzo.utils.IOU
import com.odenzo.utils.OPrint.oprint
import software.amazon.awssdk.services.apigatewayv2.ApiGatewayV2AsyncClient
import software.amazon.awssdk.services.apigatewayv2.model._
import software.amazon.awssdk.services.iam.model.Role

import scala.jdk.CollectionConverters._

object APIGateway extends AWSUtils with AwsErrorUtils {
  lazy private val client = ApiGatewayV2AsyncClient.create()

  /** A thing to make an APIGateway to a lambda function - lots hard coded */
  def createAPIGateway(name: String, desc: String, route: String, lambdaFnArn: String, lambdaRole: Role, otags: OTags)(
      implicit cs: ContextShift[IO]
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
  def createIntegration(apiId: String, lambdaFnArn: String)(implicit cs: ContextShift[IO]): IO[CreateIntegrationResponse] =
    IOU.toIO {
      client.createIntegration(
        CreateIntegrationRequest.builder
          .apiId(apiId)
          .connectionType(ConnectionType.INTERNET)
          .integrationType(IntegrationType.AWS_PROXY)
          .integrationMethod("POST") // ? thats whats reporting for existing PROD, POST to Lambda I guess
          .integrationUri(lambdaFnArn)
          .payloadFormatVersion("1.0")
          .timeoutInMillis(29000)
          // No Tags
          .build()
      )
    }

  def createRoute(apiId: String, routeKey: String)(implicit cs: ContextShift[IO]): IO[CreateRouteResponse] =
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
  def getApiIntegrations(apiId: String)(implicit cs: ContextShift[IO]): IO[List[Integration]] = {
    IOU.toIO(client.getIntegrations(GetIntegrationsRequest.builder().apiId(apiId).build())).map(_.items().asScala.toList)
  }

  /** Doesn't deal with paging. */
  def getApiRoutes(apiId: String)(implicit cs: ContextShift[IO]): IO[List[Route]] = {
    IOU.toIO(client.getRoutes(GetRoutesRequest.builder().apiId(apiId).build())).map(_.items().asScala.toList)

    // Not sure the difference, maybe singular get the "short cut" builder ingebration/
    //  client.getIntegration(GetIntegrationRequest.builder().apiId(apiId).build())
  }

  // Seems ApiID is not name. getApis has no filtering...
  def getApiGateways()(implicit cs: ContextShift[IO]): IO[List[Api]] =
    completableFutureToIO(client.getApis(GetApisRequest.builder.maxResults("200").build()))
      .map(_.items().asScala.toList)

  def getApiGateway(name: String)(implicit cs: ContextShift[IO]): IO[List[Api]] =
    getApiGateways().map(_.filter(_.name() === name))

  def getStages(apiId: String)(implicit cs: ContextShift[IO]): IO[List[Stage]] = {
    //client.getApiMapping(GetApiMappingRequest.builder().apiMappingId("x").domainName("y"))
    completableFutureToIO(client.getStages(GetStagesRequest.builder().apiId(apiId).maxResults("100").build()))
      .map(r => nullsafeFromList(r.items()))
  }

  def getIntegrationResponses(apiId: String, integrationId: String)(implicit cs: ContextShift[IO]): IO[List[IntegrationResponse]] = {
    //client.getApiMapping(GetApiMappingRequest.builder().apiMappingId("x").domainName("y"))
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
  def getDeployments(apiId: String)(implicit cs: ContextShift[IO]): IO[List[Deployment]] = {
    completableFutureToIO {
      // max is string, go figure...
      client.getDeployments(GetDeploymentsRequest.builder().apiId(apiId).maxResults("100").build())
    }.map(r => nullsafeFromList(r.items))
  }
  def deleteApiGatewaysByName(name: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      _       <- IO(scribe.debug(s"Deleting API Gateways Names $name"))
      allApis <- getApiGateways()
      apis     = allApis.filter(_.name().equals(name)).toList
      _       <- IO(scribe.debug(s"Found ${oprint(apis)}"))
      _       <- apis.traverse(v => deleteApiGateway(v.apiId))
    } yield ()
  }

  /** ID is different than name. */
  def deleteApiGateway(id: String)(implicit cs: ContextShift[IO]): IO[DeleteApiResponse] = {
    completableFutureToIO(client.deleteApi(DeleteApiRequest.builder().apiId(id).build()))
  }

}
