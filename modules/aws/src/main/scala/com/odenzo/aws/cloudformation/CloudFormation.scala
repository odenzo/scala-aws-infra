package com.odenzo.aws.cloudformation

import cats.effect._
import com.odenzo.aws.AWSUtils
import com.odenzo.utils.FS2Utils
import software.amazon.awssdk.services.cloudformation._
import software.amazon.awssdk.services.cloudformation.model.{DescribeStacksRequest, DescribeStacksResponse}

object CloudFormation extends AWSUtils {
  private val client = CloudFormationAsyncClient.create()

  def findFormation(name: String): IO[List[DescribeStacksResponse]] = {
    FS2Utils.toList(client.describeStacksPaginator(DescribeStacksRequest.builder().stackName(name).build()))
  }

}
