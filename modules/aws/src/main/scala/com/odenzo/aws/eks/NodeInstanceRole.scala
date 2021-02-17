package com.odenzo.aws.eks

import cats.effect.IO
import io.circe._
import io.circe.literal._

/** Still need to make some custom policy for the Control Plane and the Node Workers.
  * Question is if it is good enough to share policy across or worker nodes or need to bind
  * to individual services (K8S service binding to Pod type)
  **/
case class ManagedPolicy(policy: String) {
  val arn = s"arn:aws:iam::aws:policy/$policy"
}

/** Some common policies we need,including those useed for existing cluster.
  * Manually found for now */
object NodeInstanceRole {

  val awsServicePolicy = List("AWSServiceRoleForAmazonEKSNodegroup")

  // Providers ec2.amazonaws.com can assume this role
  val awsPolicies =
    List(
      "AmazonElastiCacheFullAccess",
      "AmazonEKSWorkerNodePolicy",
      "AmazonRDSDataFullAccess",
      "AmazonEC2ContainerRegistryPowerUser", // This can be refined down to R/O for cluster
      "AmazonMSKFullAccess",
      "AmazonEKS_CNI_Policy",                // Networking CNI needed
      "AmazonKeyspacesFullAccess"            // Ideally scope down to cluster keyspace
      // Plus our custom policies
    )

  val autoScaling = json"""{
                                    "Version": "2012-10-17",
                                    "Statement": [
                                        {
                                            "Action": [
                                                "autoscaling:DescribeAutoScalingGroups",
                                                "autoscaling:DescribeAutoScalingInstances",
                                                "autoscaling:DescribeLaunchConfigurations",
                                                "autoscaling:DescribeTags",
                                                "autoscaling:SetDesiredCapacity",
                                                "autoscaling:TerminateInstanceInAutoScalingGroup",
                                                "ec2:DescribeLaunchTemplateVersions"
                                            ],
                                            "Resource": "*",
                                            "Effect": "Allow"
                                        }
                                    ]
                                }"""

  val externalDNSChangeSet = json"""
      {
          "Version": "2012-10-17",
          "Statement": [
              {
                  "Action": [
                      "route53:ChangeResourceRecordSets"
                  ],
                  "Resource": "arn:aws:route53:::hostedzone/*",
                  "Effect": "Allow"
              }
          ]
      }
      """

  val externalDNSHostedZones = json"""
      {
          "Version": "2012-10-17",
          "Statement": [
              {
                  "Action": [
                      "route53:ListHostedZones",
                      "route53:ListResourceRecordSets"
                  ],
                  "Resource": "*",
                  "Effect": "Allow"
              }
          ]
      }
      """

  /** Buckets i list of names or can be wildcard like * / * too.
    * Used to restrict to buckets for this clustername usually.
    * */
  def workerPolicyForS3Buckets(buckets: List[String]): IO[Json] = {
    val bucketAccess = buckets.map(b => s""" "arn:aws:s3:::$b" """).mkString("", ",\n", "\n")
    val policy       =
      s""""{
         | Version": "2012-10-17",
         | "Statement": [
         |     {
         |      "Action": "S3:*",
         |      "Resource": [
         |          $bucketAccess

         |}""".stripMargin
    IO.fromEither(parser.parse(policy))
  }

  // A few more of these policies may be needed, e.g. Postgres and Kafka... combine into one ManagedServicesPolicy
}
