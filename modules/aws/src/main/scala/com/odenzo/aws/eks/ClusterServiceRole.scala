package com.odenzo.aws.eks

import io.circe.literal._

/** Standard Policies for the Cluster ServiceControlPlane role */
object ClusterServiceRole {

  val serviceRole = List("AWSServiceRoleForAmazonEKSNodegroup")

  /** Base role every cluster needs. Typically just this and the ASSServiceRoleFoAmazonEKSNodegroup */
  val nlb = json"""{
               "Version": "2012-10-17",
               "Statement": [
                   {
                       "Action": [
                           "elasticloadbalancing:*",
                           "ec2:CreateSecurityGroup",
                           "ec2:Describe*"
                       ],
                       "Resource": "*",
                       "Effect": "Allow"
                   }
               ]
           }"""

  val cloudWatchMetrics = json"""{
                                    "Version": "2012-10-17",
                                    "Statement": [
                                        {
                                            "Action": [
                                                "cloudwatch:PutMetricData"
                                            ],
                                            "Resource": "*",
                                            "Effect": "Allow"
                                        }
                                    ]
                                }"""
}
