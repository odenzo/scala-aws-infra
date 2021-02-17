package com.odenzo.aws.iam

import io.circe.literal._

/** When creating a role, have to say who can assume that role, this has some common ones for our environment */
object TrustRelationships {

  val rootAdmin = json"""{
                       "Version": "2012-10-17",
                       "Statement": [
                         {
                           "Effect": "Allow",
                           "Principal": {
                             "AWS": "arn:aws:iam::879130378853:root"
                           },
                           "Action": "sts:AssumeRole",
                           "Condition": {}
                         }
                       ]
                     }"""

  /** EKS and Fargate -- for the K8S Cluster Service Role */
  val eks = json"""{
                     "Version": "2012-10-17",
                     "Statement": [
                       {
                         "Effect": "Allow",
                         "Principal": {
                           "Service": ["eks.amazonaws.com","eks-fargate-pods.amazonaws.com"]

                         },
                         "Action": "sts:AssumeRole"
                       }
                     ]
                   }"""

  /** EC2 for the K8S Cluster Node Group Role */
  val ec2 = json"""{
                     "Version": "2012-10-17",
                     "Statement": [
                       {
                         "Effect": "Allow",
                         "Principal": {
                           "Service": "ec2.amazonaws.com"
                         },
                         "Action": "sts:AssumeRole"
                       }
                     ]
                   }"""

  val awsLambda =
    json"""{
                     "Version": "2012-10-17",
                     "Statement": [
                       {
                         "Effect": "Allow",
                         "Principal": {
                           "Service": "lambda.amazonaws.com"
                         },
                         "Action": "sts:AssumeRole"
                       }
                     ]
                   }"""
}

object AWSRoles {
  val fargate      = "arn:aws:iam::879130378853:role/AmazonEKSFargatePodExecutionRole"
  val eksNodegroup = "arn:aws:iam::879130378853:role/aws-service-role/eks-nodegroup.amazonaws.com/AWSServiceRoleForAmazonEKSNodegroup"

}
