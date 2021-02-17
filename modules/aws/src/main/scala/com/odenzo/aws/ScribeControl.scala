package com.odenzo.aws

import com.odenzo.utils.ScribeConfig
import scribe.{Level, Logger, Priority}

object ScribeControl {

  val toInfo: List[String] = List(
    "software.amazon.awssdk.auth",
    "software.amazon.awssdk.core.interceptor",
    "io.netty",
    "software.amazon.awssdk",
    "com.datastax.oss.driver.internal",
    "org.http4s.blaze",
    "org.http4s.client"
  )

  val toWarn = List("com.datastax.oss.driver")

  def sush(): Logger = {
    val filter = ScribeConfig.setLevelOnPackages(toInfo, Level.Info, Priority.High)
    ScribeConfig.applyFilter(filter)

    val f2 = ScribeConfig.setLevelOnPackages(toWarn, Level.Warn, Priority.Highest)
    ScribeConfig.applyFilter(f2)
  }

  def debugMine: Logger = {
    val mine   = List("com.odenzo", "co.horn")
    val filter = ScribeConfig.setLevelOnPackages(mine, Level.Debug, Priority.High)
    ScribeConfig.applyFilter(filter)
  }

  def stdConfig: Logger = {
    ScribeConfig.resetAllToLevel(Level.Debug)
    sush()
  }

}
