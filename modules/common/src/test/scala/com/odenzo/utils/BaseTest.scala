package com.odenzo.utils

import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{EitherValues, OptionValues}
import scribe.Level

import scala.concurrent.ExecutionContext

trait BaseTest extends AnyFunSuite with Matchers with EitherValues with OptionValues {

  implicit val defaultContextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val defaultTimer: Timer[IO]               = IO.timer(ExecutionContext.global)

  def logAtDebug() = com.odenzo.utils.ScribeConfig.resetAllToLevel(Level.Debug)

}
