package com.odenzo.utils

import cats.effect.unsafe.IORuntime
import munit.FunSuite
import scribe.Level

abstract class BaseTest extends FunSuite {
  implicit val IOR: IORuntime = IORuntime.global
  def logAtDebug()            = com.odenzo.utils.ScribeConfig.resetAllToLevel(Level.Debug)

}
