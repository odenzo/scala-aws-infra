package com.odenzo.utils

import cats.effect.IO
import cats.syntax.all._
import com.odenzo.utils.CommandLineArgs.Args
import com.odenzo.utils.OPrint.oprint
import geny.ByteData
import monocle.macros.Lenses
import os.{CommandResult, Shellable}

import java.io.File

object CommandLineExecutor {

  /** Exceutes a command in cwd with given environment variables in addition to inherited. No stdin piping etc */
  def execute(cmdLine: Shellable, env: Map[String, String] = Map.empty): CommandResult = {
    os.proc(cmdLine).call(env = env, check = true, mergeErrIntoOut = false, propagateEnv = true)
  }

}
