package com.odenzo.aws.testutils

import cats.effect.{ContextShift, IO, Timer}
import com.odenzo.aws.{OTags, ScribeControl}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{EitherValues, OptionValues}
import scribe.Level
import software.amazon.awssdk.regions.Region

import scala.concurrent.ExecutionContext

trait AWSBaseTest extends AnyFunSuite with Matchers with EitherValues with OptionValues {

  val globalTags: OTags =
    OTags.from("Name" -> "k8s/installer-tests", "Project" -> "Dev", "DeleteMe" -> "true", "SubProject" -> "PlayGround")

  val region            = Region.US_EAST_1

  implicit val defaultContextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val defaultTimer: Timer[IO]               = IO.timer(ExecutionContext.global)

  def logAtDebug() = com.odenzo.utils.ScribeConfig.resetAllToLevel(Level.Debug)

  def quietPlease = ScribeControl.sush()

  logAtDebug()
  quietPlease
}
