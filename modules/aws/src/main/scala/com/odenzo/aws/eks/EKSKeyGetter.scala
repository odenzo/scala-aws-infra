//package com.odenzo.aws.eks
//
//class EKSKeyGetter {
//
//}
//
//import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider
//import software.amazon.awssdk.auth.signer.Aws4Signer
//import software.amazon.awssdk.auth.signer.params.Aws4PresignerParams
//import software.amazon.awssdk.http.SdkHttpFullRequest
//import software.amazon.awssdk.http.SdkHttpMethod
//import java.time.{Clock, Instant, ZonedDateTime}
//import java.time.temporal.{ChronoUnit, Temporal, TemporalUnit}
//import java.util.Base64
//
//import cats._
//import cats.data._
//import cats.syntax.all._
//import cats.effect._
//import software.amazon.awssdk.regions.Region
//
//object Foo {
//def getAuthenticationToken (awsAuth: AwsCredentialsProvider, awsRegion: Region, clusterName: String): String = {
//
//
//    val expirationDate : ZonedDateTime       = ZonedDateTime.now().plus(60, ChronoUnit.SECONDS)
//    val presignerParams: Aws4PresignerParams = Aws4PresignerParams.builder
//      .awsCredentials(awsAuth.resolveCredentials)
//      .signingRegion(awsRegion)
//      .signingName("sts")
//      .signingClockOverride(Clock.systemUTC)
//      .expirationTime(expirationDate.toInstant)
//      .build
//
//
//    val signRq =  for {
//      uri <- getStsRegionalEndpointUri(awsRegion)
//      rq = SdkHttpFullRequest
//        .builder
//        .method(SdkHttpMethod.GET)
//        .uri(uri)
//        .appendHeader("x-k8s-aws-id", clusterName)
//        .appendRawQueryParameter("Action", "GetCallerIdentity")
//        .appendRawQueryParameter("Version", "2011-06-15")
//        .build
//
//      signedRequest = Aws4Signer.create.presign(rq, presignerParams)
//      encUrl =  Base64.getUrlEncoder.withoutPadding.encodeToString(signedRequest.getUri.toString.getBytes("UTF-8"))
//    }            yield s"k8s-aws-v1.$encUrl"
//
//
//  signRq.
//  case e: Exception =>
//  val errorMessage: String = "A problem occurred generating an Eks authentication token for cluster: " + clusterName
//  scribe.error(errorMessage, e)
//  throw new RuntimeException (errorMessage, e)
//}
//
//  import java.net.URI
//  import java.net.URISyntaxException
//  def getStsRegionalEndpointUri(awsRegion: Region) = {
//    IO(new URI("https", s"sts.${awsRegion.id}.amazonaws.com", "/", null)).adaptErr {
//    case shouldNotHappen: URISyntaxException =>
//      val errorMessage = "An error occurred creating the STS regional endpoint Uri"
//      throw new RuntimeException(errorMessage, shouldNotHappen)
//  }
//  }
//}
//
