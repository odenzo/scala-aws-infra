package com.odenzo.aws.testutils

import cats.effect.unsafe.IORuntime
import com.odenzo.aws.{OTags, ScribeControl}
import munit.FunSuite
import scribe.Level
import software.amazon.awssdk.regions.Region

trait AWSBaseTest extends FunSuite {

  val globalTags: OTags =
    OTags.from("Name" -> "k8s/installer-tests", "Project" -> "Dev", "DeleteMe" -> "true", "SubProject" -> "PlayGround")

  val region            = Region.US_EAST_1

  implicit val IOR = IORuntime.global

  def logAtDebug() = com.odenzo.utils.ScribeConfig.resetAllToLevel(Level.Debug)

  def quietPlease = ScribeControl.sush()

  logAtDebug()
  quietPlease
}
