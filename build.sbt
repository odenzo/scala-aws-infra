import MyCompileOptions._

ThisBuild / organization   := "com.odenzo"
ThisBuild / scalaVersion   := "2.13.4"
ThisBuild / fork in run    := true
Global / semanticdbVersion := "0.0.1"
bspEnabled                 := false
Test / logBuffered         := true
Test / parallelExecution   := false

// -release 11 etc is only on Java 9 or higher but Scala 2.13 supports -release
lazy val compileSettings =
  Seq(
    scalacOptions ++= Seq("-release", "11") ++ optsV13 ++ warningsV13 ++ lintersV13
    //javacOptions ++= Seq("-source", "1.11", "-target", "1.8")
  )

lazy val commonSettings = Seq(
  libraryDependencies ++= libs ++ lib_circe ++ lib_cats ++ lib_chimney ++ lib_monocle ++ lib_scribe ++  lib_fs2 ++ libs_test,
  libraryDependencies ++= Seq("com.lihaoyi" %% "pprint" % pprintV)
)

// Aggregate them all for easy dev and make sure no errors in unused modules
lazy val root =
  project
    .in(file("."))
    .aggregate(common, aws)

lazy val common = project
  .in(file("modules/common"))
  .settings(commonSettings, compileSettings)

// ===== Service Specific Libraries that contain both business logic and examle/supporting code now.
// Business logic should be pushed up to applications service layer where possible
// And these should be like utility/supporting modules only
// Sometimes they have additional junk that I forget... simple example code snippets etc.
//
lazy val aws = project
  .in(file("modules/aws"))
  .dependsOn(common)
  .settings(
    compileSettings,
    commonSettings,
    libraryDependencies ++= lib_aws ++ lib_fs2 ++ libs_test //++ lib_avro
  )

//========== Applications ==============

// ========== Libraries (Time to Go Elsewhere) ==============

resolvers += "Confluent Maven Repository" at "https://packages.confluent.io/maven/"


val awsSdk                   = "2.15.72"
val catsEffectV              = "2.3.1"
val catsV                    = "2.3.1"
val chimneyVersion           = "0.6.1"
val circeConfigV             = "0.8.0"
val circeVersion             = "0.13.0"
val circeYamlV               = "0.13.1"
val fs2V                     = "2.5.0"
val logbackV                 = "1.2.3"
val monocleV                 = "2.1.0"
val oslibV                   = "0.7.2"
val pprintV                  = "0.6.1"
val pureSchedularV           = "0.4.3"
val scalaCheckV              = "1.15.2"
val scalaMockV               = "5.0.0"
val scalaTestV               = "3.2.3"
val scribeV                  = "3.3.1"
val declineV                 = "1.3.0"

val libs = {
  Seq(
    "com.lihaoyi"  %% "pprint"         % pprintV,
    "com.lihaoyi"  %% "os-lib"         % oslibV,
  )
}

val libs_test = Seq("org.scalatest" %% "scalatest" % scalaTestV % Test, "org.scalacheck" %% "scalacheck" % scalaCheckV % Test)

val lib_aws = Seq(
  //"software.amazon.awssdk" % "cassandra"   % awsSdk,
  "software.amazon.awssdk" % "acm"            % awsSdk,
  "software.amazon.awssdk" % "apigatewayv2"   % awsSdk,
  "software.amazon.awssdk" % "cloudformation" % awsSdk,
  "software.amazon.awssdk" % "ec2"            % awsSdk,
  "software.amazon.awssdk" % "ecr"            % awsSdk,
  "software.amazon.awssdk" % "elasticsearch"  % awsSdk,
  "software.amazon.awssdk" % "eks"            % awsSdk,
  "software.amazon.awssdk" % "elasticache"    % awsSdk,
  "software.amazon.awssdk" % "iam"            % awsSdk,
  "software.amazon.awssdk" % "kafka"          % awsSdk,
  "software.amazon.awssdk" % "kms"            % awsSdk,
  "software.amazon.awssdk" % "lambda"         % awsSdk,
  "software.amazon.awssdk" % "rds"            % awsSdk,
  "software.amazon.awssdk" % "rdsdata"        % awsSdk,
  "software.amazon.awssdk" % "route53"        % awsSdk,
  "software.amazon.awssdk" % "route53domains" % awsSdk,
  "software.amazon.awssdk" % "s3"             % awsSdk,
  "software.amazon.awssdk" % "secretsmanager" % awsSdk,
  "software.amazon.awssdk" % "sns"            % awsSdk,
  "software.amazon.awssdk" % "ssm"            % awsSdk,
  "software.amazon.awssdk" % "sts"            % awsSdk
)

val lib_fs2 = Seq(
  "co.fs2" %% "fs2-core"             % fs2V,
  "co.fs2" %% "fs2-reactive-streams" % fs2V
  //"io.laserdisc" %% "fs2-aws"              % "VERSION"
)

val lib_chimney = Seq("io.scalaland" %% "chimney" % chimneyVersion, "io.scalaland" %% "chimney-cats" % chimneyVersion)

// Similar to cats retry but publihed
val lib_pure_scuedular = Seq("com.emarsys" %% "scheduler" % pureSchedularV)

/** JSON Libs == Circe and Associated Support Libs */
val lib_circe = Seq(
  "io.circe" %% "circe-core"           % circeVersion,
  "io.circe" %% "circe-generic"        % circeVersion,
  "io.circe" %% "circe-parser"         % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics"         % circeVersion,
  "io.circe" %% "circe-literal"        % circeVersion,
  "io.circe" %% "circe-yaml"           % circeYamlV,
  "io.circe" %% "circe-config"         % circeConfigV
)


val lib_cats   = Seq("org.typelevel" %% "cats-core" % catsV, "org.typelevel" %% "cats-effect" % catsEffectV)

val lib_monocle = Seq("com.github.julien-truffaut" %% "monocle-core" % monocleV, "com.github.julien-truffaut" %% "monocle-macro" % monocleV)

val lib_scribe = Seq("com.outr" %% "scribe" % scribeV, "com.outr" %% "scribe-slf4j" % scribeV)
