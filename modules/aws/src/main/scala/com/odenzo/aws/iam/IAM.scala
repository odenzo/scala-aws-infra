package com.odenzo.aws.iam

import cats._
import cats.data._
import cats.effect.{ContextShift, IO}
import cats.syntax.all._
import com.odenzo.aws.{AWSUtils, AwsErrorUtils, OTag, OTags}
import com.odenzo.utils.{FS2Utils, IOU}
import io.circe.Json
import fs2._
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.iam.IamAsyncClient
import software.amazon.awssdk.services.iam.model.{ListUsersRequest, _}

import java.util.concurrent.CompletionException
import scala.jdk.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps

/** Whoever is calling this has an IAM role, but for each cluster we create a new IAM role.
  * THis will have permissisons and also be used to issue service specific password (e.g. Cassandra)
  */
object IAM extends AwsErrorUtils with AWSUtils {

  /** Autoclosing Session Client MT Safe */
  final lazy val client: IamAsyncClient = IamAsyncClient.builder().region(Region.AWS_GLOBAL).build()

  private[iam] val toIamTag: (String, String) => Tag = (k: String, v: String) => Tag.builder().key(k).value(v).build()
  private[iam] val otagToIamTag: OTag => Tag         = (otag) => Tag.builder().key(otag.name).value(otag.content).build()

  /** This will delete all the attached policies (if any) then delete the rolw */
  def deleteIamUser(userName: String)(implicit cs: ContextShift[IO]): IO[DeleteUserResponse] = {
    IOU
      .toIO {
        client.deleteUser(DeleteUserRequest.builder.userName(userName).build())
      } >>= resultSuccessful(s"Deleting IAM $userName")
  }

  /** This detaches all policies and deletes the IAM user, if it already exists.
    * This complains to delete referenced objects, but not sure why: No VPC, no AccessKey, no polies attached.
    * Simple delete in console works though.... sigh...
    */
  def deleteIamUserIfExists(userName: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    findIamUser(userName).flatMap {
      case None      => IO(scribe.debug(s"No Existing IAM User: $userName"))
      case Some(usr) =>
        for {
          _ <- listAttachedUserPolicies(userName).flatMap(_.traverse(policy => detachUserPolicy(userName, policy.policyArn)))
          _ <- listAccessKeys(userName).flatMap(_.traverse(akm => deleteAccessKey(userName, akm.accessKeyId)))
          _ <- deleteAllServiceSpecificCredentials(userName)
          _ <- IO(scribe.info(s"About to Delete IAM User ${usr.userName}")) *> deleteIamUser(usr.userName)
        } yield ()
    }

  }

  def createIamUser(userName: String, path: String = "/k8s/", tags: OTags)(implicit cs: ContextShift[IO]): IO[User] = {
    IOU
      .toIO(client.createUser(CreateUserRequest.builder.userName(userName).path(path).tags(tags.via(toIamTag)).build()))
      .map(_.user())
  }

  def getCurrentUser()(implicit cs: ContextShift[IO]): IO[User] = {
    IOU.toIO(client.getUser()).map(_.user())
  }

  def getIamUser(userName: String)(implicit cs: ContextShift[IO]): IO[User] = {
    IOU
      .toIO(client.getUser(GetUserRequest.builder.userName(userName).build))
      .map(_.user())
  }

  def findIamUser(userName: String)(implicit cs: ContextShift[IO]): IO[Option[User]] = {
    getIamUser(userName)
      .redeem(AwsErrorUtils.nestedRecoverToOption[NoSuchEntityException], u => u.some)
  }

  def getOrCreateUser(user: String, path: String, tags: OTags)(implicit cs: ContextShift[IO]): IO[User] = {
    getIamUser(user)
      .recoverWith {
        // Impedence squared.
        case ex: CompletionException if ex.getCause.isInstanceOf[NoSuchEntityException] =>
          scribe.info(s"No existing cluster IAM -- Making one for $user")
          IAM.createIamUser(user, path, tags)
      }
  }

  def listRoleTags(roleName: String)(implicit cs: ContextShift[IO]): IO[List[Tag]] = {
    IOU
      .toIO(client.listRoleTags(ListRoleTagsRequest.builder().roleName(roleName).maxItems(100).build()))
      .map(_.tags().asScala.toList)
  }

  def getPolicyByArn(arn: String)(implicit cs: ContextShift[IO]): IO[Policy] = {
    IOU
      .toIO(client.getPolicy(GetPolicyRequest.builder().policyArn(arn).build()))
      .map(_.policy())
  }

  def findNamedPolicy(name: String)(implicit cs: ContextShift[IO]): IO[Option[Policy]] = {
    for {
      policies <- listPolicies()
      matches   = policies.filter(_.policyName().equals(name))
      result   <- matches.compile.last
    } yield result
  }

  def getNamedPolicy(name: String)(implicit cs: ContextShift[IO]): IO[Policy] = {
    findNamedPolicy(name) >>= IOU.required(s"Policy $name Not Found")
  }

  /** This gets the attached policies, but not the inline polocies */
  def listAttachedUserPolicies(userName: String)(implicit cs: ContextShift[IO]): IO[List[AttachedPolicy]] =
    IOU
      .toIO(client.listAttachedUserPolicies(ListAttachedUserPoliciesRequest.builder.userName(userName).build()))
      .map(_.attachedPolicies().asScala.toList)

  /** @return List of User Policies */
  def listUserPolicies(userName: String)(implicit cs: ContextShift[IO]): IO[List[String]] =
    IOU
      .toIO(client.listUserPolicies(ListUserPoliciesRequest.builder.userName(userName).build()))
      .map(_.policyNames().asScala.toList)

  def listPolicies()(implicit cs: ContextShift[IO]): IO[Stream[IO, Policy]] = {
    val rq = ListPoliciesRequest.builder.scope(PolicyScopeType.ALL).build()

    for {
      stream <- FS2Utils.toStream(client.listPoliciesPaginator(rq))
      content = stream.map(_.policies().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  /** See also create policy version. */
  def createPolicy(name: String, path: String, desc: String, policy: Json)(implicit cs: ContextShift[IO]): IO[Policy] = {
    IOU
      .toIO(
        client.createPolicy(
          CreatePolicyRequest.builder
            .policyName(name)
            .path(path)
            .description(desc)
            .policyDocument(policy.spaces2)
            .build()
        )
      )
      .map(_.policy())
  }

  def createPolicyVersion(arn: String, policy: Json, asDefault: Boolean)(implicit cs: ContextShift[IO]): IO[PolicyVersion] = {
    IOU
      .toIO(
        client.createPolicyVersion(
          CreatePolicyVersionRequest.builder
            .policyArn(arn)
            .policyDocument(policy.spaces2)
            .setAsDefault(asDefault)
            .build()
        )
      )
      .map(_.policyVersion())
  }

  /** Attach existing policy to IAM user directly */
  def attachUserPolicy(userName: String, policyArn: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    IOU
      .toIO(client.attachUserPolicy(AttachUserPolicyRequest.builder.userName(userName).policyArn(policyArn).build()))
      .void
  }

  /** This still leaves some object related to the IAM, not sure what. */
  def detachUserPolicy(userName: String, policyArn: String)(implicit cs: ContextShift[IO]): IO[Unit] =
    IO(scribe.debug(s"Detaching IAM User Policy: $userName - $policyArn")) *>
      IOU.toIO(client.detachUserPolicy(DetachUserPolicyRequest.builder.userName(userName).policyArn(policyArn).build)).void

  /** Attach existing policy to a role, e.g. one of the AWS Policies, by ARN */
  def attachRolePolicy(roleName: String, policyArn: String)(implicit cs: ContextShift[IO]): IO[Boolean] = {
    IO(scribe.debug(s"Attaching Policy Arn: $policyArn to Role  $roleName")) *>
      IOU
        .toIO {
          scribe.debug(s"Adding Policy ARM $policyArn to AWS IAM ROle $roleName")
          client.attachRolePolicy(AttachRolePolicyRequest.builder.roleName(roleName).policyArn(policyArn).build())
        }
        .map(_.sdkHttpResponse().isSuccessful)
  }

  /** Not sure if these are just the inline ones, or include the attached ones. */
  def listRolePolicies(roleName: String)(implicit cs: ContextShift[IO]): IO[Stream[IO, String]] = {
    FS2Utils
      .toBurstStream(client.listRolePoliciesPaginator(ListRolePoliciesRequest.builder.roleName(roleName).build()))(_.policyNames.asScala)
  }

  def listRoleAttachedPolicies(roleName: String)(implicit cs: ContextShift[IO]): IO[Stream[IO, AttachedPolicy]] = {
    FS2Utils
      .toBurstStream(client.listAttachedRolePoliciesPaginator(ListAttachedRolePoliciesRequest.builder.roleName(roleName).build()))(
        _.attachedPolicies.asScala
      )

  }

  /** Nothing of interest returned */
  def attachRoleInlinePolicy(roleName: String, policyName: String, policy: Json)(
      implicit cs: ContextShift[IO]
  ): IO[PutRolePolicyResponse] = {
    IOU.toIO(
      client.putRolePolicy(
        PutRolePolicyRequest.builder
          .roleName(roleName)
          .policyName(policyName)
          .policyDocument(policy.spaces2)
          .build()
      )
    )
  }

  def detachRolePolicy(roleName: String, policyArn: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    // This gets a 404 if not found as a policy attached
    // Guess I will check to see if its attached first
    val findExisting: IO[Option[AttachedPolicy]] = listRoleAttachedPolicies(roleName).flatMap { stream =>
      stream.filter(_.policyArn() == policyArn).compile.last
    }

    val detachPolicy = IOU.toIO {
      scribe.info(s"Detaching Policy ARN: $policyArn from Role Name $roleName")
      client.detachRolePolicy(DetachRolePolicyRequest.builder.roleName(roleName).policyArn(policyArn).build())
    }.void

    for {
      existing <- findExisting
      _        <- IO(scribe.info(s"Role $roleName has Polocy $policyArn? ->  $existing"))
      _        <- detachPolicy.whenA(existing.isDefined)
    } yield ()
  }

  /** Finds a role by roleName, not by the Tag Name:<roleName> */
  def findRole(name: String)(implicit cs: ContextShift[IO]): IO[Option[Role]] = {
    IOU
      .toIO(client.getRole(GetRoleRequest.builder.roleName(name).build()))
      .map(_.role())
      .redeem(AwsErrorUtils.nestedRecoverToOption[NoSuchEntityException], _.some)
  }

  /** Returns optinally one matching IAM role. If multiple found error is raised. */
  def findRoleByTag(tag: OTag, startsWith: Option[String], pathPrefix: String = "/")(implicit cs: ContextShift[IO]): IO[Option[Role]] = {
    val awsTag: Tag = otagToIamTag(tag)
    listRoles(pathPrefix).flatMap { stream =>
      stream
        .filter(r => startsWith.forall(s => r.roleName().startsWith(s)))
        .debug(r => "EKS: " + r.roleName() + s"  ${r.tags}", scribe.info(_))
        .evalFilterAsync(10)(rn => this.listRoleTags(rn.roleName()).map(tags => tags.contains(awsTag)))
        .debug("TAGED OK: " + _.roleName(), scribe.info(_))
        .compile
        .toList
    } >>= IOU.optionOne(s"IAM Role with tag $tag")
  }

  /** List roles with optional path, eg k8s/cluster/  Forget if I still use paths think not. eksctl doesn't for sure. */
  def listRoles(pathPrefix: String = "/")(implicit cs: ContextShift[IO]): IO[Stream[IO, Role]] = {
    val request = ListRolesRequest.builder.pathPrefix(pathPrefix).build
    for {
      stream <- FS2Utils.toStream(client.listRolesPaginator(request))
      content = stream.map(_.roles().asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst

  }

//  def findResourceByArn(arn: String)(implicit cs: ContextShift[IO]): IO[Option[Role]] = {
//    IOUtils
//      .toIO(client.getRole(GetRoleRequest.builder.roleName(name).build()))
//      .map(_.role())
//      .redeem(AwsErrorUtils.nestedRecoverToOption[NoSuchEntityException], _.some)
//
//  }

  /** Note: Returned ROle Polucy is URL Encoded JSON LOL */
  def createRole(clusterName: String, roleName: String, trustRelationships: Json, tags: OTags)(implicit cs: ContextShift[IO]): IO[Role] = {
    IOU
      .toIO(
        client.createRole(
          CreateRoleRequest.builder
            .roleName(roleName)
            .path(s"/k8s/$clusterName/")
            .description(s"$clusterName Role")
            .assumeRolePolicyDocument(trustRelationships.noSpaces)
            .tags(tags.via(toIamTag))
            .build()
        )
      )
      .map(_.role)
  }

  def deleteRoleAndPoliciesIfExists(roleName: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    findRole(roleName).flatMap {
      case None       => IO(scribe.info(s"Role $roleName not found")).void
      case Some(role) =>
        for {
          // Don't think we have to delete inline policies
          //    _ <- listRolePolicies(roleName).flatMap(stream => stream.parEvalMap(4)(p => deletePolicy(p))
          _ <- IO(scribe.info(s"Deleting Role $role"))
          _ <- listRoleAttachedPolicies(roleName).flatMap(
                 stream => stream.parEvalMap(4)(ap => detachRolePolicy(roleName, ap.policyArn)).compile.toList
               )
          _ <- deleteRole(role.roleName())
        } yield ()
    }
  }

  /** This will probably throw an error if role doesn't exist and also all the policies must be deleted. */
  def deleteRole(roleName: String)(implicit cs: ContextShift[IO]): IO[DeleteRoleResponse] = {
    IOU.toIO(client.deleteRole(DeleteRoleRequest.builder.roleName(roleName).build()))

  }

  /** List Users streaming all responses back with just the users (bursted for inefficiency)
    * TODO: Add filters
    */
  def listUsers()(implicit cs: ContextShift[IO]): IO[Stream[IO, User]] = {
    val request = ListUsersRequest.builder.build
    for {
      stream <- FS2Utils.toStream(client.listUsersPaginator(request))
      content = stream.map(_.users.asScala.toList)
      burst   = content.flatMap(Stream.emits)
    } yield burst
  }

  def createServiceSpecificCredentials(user: String, service: String)(implicit cs: ContextShift[IO]): IO[ServiceSpecificCredential] = {
    completableFutureToIO(
      client.createServiceSpecificCredential(
        CreateServiceSpecificCredentialRequest.builder
          .serviceName(service)
          .userName(user)
          .build()
      )
    ).map(_.serviceSpecificCredential)
  }

  def listServiceSpecificCredentials(user: String, service: String)(
      implicit cs: ContextShift[IO]
  ): IO[List[ServiceSpecificCredentialMetadata]] = {
    IOU
      .toIO(
        client.listServiceSpecificCredentials(
          ListServiceSpecificCredentialsRequest.builder
            .serviceName(service)
            .userName(user)
            .build()
        )
      )
      .map(_.serviceSpecificCredentials().asScala.toList)
  }

  def listServiceSpecificCredentials(user: String, service: Option[String] = None)(
      implicit cs: ContextShift[IO]
  ): IO[List[ServiceSpecificCredentialMetadata]] = {
    val rq = ListServiceSpecificCredentialsRequest.builder.userName(user).pipe(rq => service.map(rq.serviceName).getOrElse(rq)).build()
    IOU.toIO(client.listServiceSpecificCredentials(rq)).map(_.serviceSpecificCredentials().asScala.toList)
  }

  def deleteAllServiceSpecificCredentials(user: String)(implicit cs: ContextShift[IO]): IO[Unit] = {
    for {
      ss <- listServiceSpecificCredentials(user, None)
      _  <- ss.traverse(md => this.deleteServiceSpecificCredential(user, md.serviceSpecificCredentialId()))
    } yield ()
  }

  def deleteServiceSpecificCredential(user: String, credentialId: String)(
      implicit cs: ContextShift[IO]
  ): IO[DeleteServiceSpecificCredentialResponse] = {
    completableFutureToIO {
      client.deleteServiceSpecificCredential(
        DeleteServiceSpecificCredentialRequest.builder
          .userName(user)
          .serviceSpecificCredentialId(credentialId)
          .build()
      )
    }
  }

  def resetServiceCredential(user: String, credentialId: String)(implicit cs: ContextShift[IO]): IO[ServiceSpecificCredential] = {
    IOU
      .toIO(
        client
          .resetServiceSpecificCredential(
            ResetServiceSpecificCredentialRequest.builder
              .userName(user)
              .serviceSpecificCredentialId(credentialId)
              .build()
          )
      )
      .map(_.serviceSpecificCredential)
  }

  def listAccessKeys(userName: String)(implicit cs: ContextShift[IO]): IO[List[AccessKeyMetadata]] = {
    FS2Utils.toBurstList(client.listAccessKeysPaginator(ListAccessKeysRequest.builder.userName(userName).build()))(
      v => v.accessKeyMetadata.asScala
    )

  }

  def deleteAccessKey(user: String, keyId: String)(implicit cs: ContextShift[IO]): IO[Unit] =
    IOU.toIO(client.deleteAccessKey(DeleteAccessKeyRequest.builder.userName(user).accessKeyId(keyId).build())).void

  /** We can create many access keys, not sure the limit. Generally when tear down cluster we delete them all */
  def createAccesKey(userName: String)(implicit cs: ContextShift[IO]): IO[AccessKey] = {
    completableFutureToIO(client.createAccessKey(CreateAccessKeyRequest.builder.userName(userName).build()))
      .map(_.accessKey)
  }
}
