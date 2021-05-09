package com.odenzo.aws.apigatewayv2

import cats.effect._
import cats.syntax.all._
import com.odenzo.aws.apigateway.APIGateway
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.IOU
import com.odenzo.utils.OPrint.oprint

class ApiGatewayTest extends AWSBaseTest {

  test("List API") {
    val result = for {
      gws <- APIGateway.getApiGateways()

      res <- gws.toList.traverse { gw =>
               for {
                 id           <- IO.pure(gw.apiId())
                 is           <- APIGateway.getApiIntegrations(id)
                 routs        <- APIGateway.getApiRoutes(id)
                 stages       <- APIGateway.getStages(id)
                 deploys      <- APIGateway.getDeployments(id)
                 intResponses <- is.traverse(intg => APIGateway.getIntegrationResponses(id, intg.integrationId()))
               } yield (gw, is, routs, stages, deploys, intResponses)
             }
    } yield res
    val out    = result.unsafeRunSync()
    scribe.debug(s"R2 ${oprint(out)}")
  }

  test("Get API Gateway by Name") {
    val res = APIGateway.getApiGateway("k8s-img-api-dev").unsafeRunSync()
    scribe.debug(oprint(res))
  }

  test("By Name Integration") {
    val apiName = "some-api"

    val prog = for {
      api    <- APIGateway.getApiGateway(apiName) >>= IOU.exactlyOne(s"Gateway: $apiName")
      routes <- APIGateway.getApiRoutes(api.apiId)
      _      <- IO(scribe.debug(s"Routes (All): ${oprint(routes)}"))
      ints   <- APIGateway.getApiIntegrations(api.apiId)
      _      <- IO(scribe.debug(s"Integrations (All): ${oprint(ints)}"))
    } yield ()
    prog.unsafeRunSync()
  }

}
