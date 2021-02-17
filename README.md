# scala-aws-infra
AWS SDK 2 Scala Bridge with a focus on setting up AWS Resources

These are a collection of (unprincipled) helper functions to deal with AWS Java SDK V2.0. Focus is primarily on AWS APIs to create and
administer rather than application libraries.
Uses fs2 and cats  with Scala 2.13 currently.


## Main Focus
- EC2: VPC, Security Groups 
- ACM
- IAM
- ECR
- KAFKA (MKS)
- Redis
- RDS: Postgres and some Aurora
- S3: Setup, not runtime actions
- EKS: Via eksctl mostly

AWS Docs Link: https://docs.aws.amazon.com/index.html
      


