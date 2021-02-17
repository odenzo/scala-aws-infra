package com.odenzo.utils

import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.utils.OPrint.oprint
import monocle.macros.Lenses

import java.io.File

object CommandLine {

  case class CmdLineResult(exitCode: Int, stdout: Option[String], stderr: Option[String]) {
    override def toString: String = s"""
                                       |
                                       | Command Result: $exitCode
                                       | \nSTDOUT:\n${stdout.getOrElse("<EMPTY>")}
                                       | \nSTDERR:\n${stderr.getOrElse("<EMPTY>")}
                                       |""".stripMargin

    def isError: Boolean = exitCode != 0
  }

  trait CmdLineArg {
    def toZsh: String
  }
  case class UnaryArg(arg: String)                extends CmdLineArg {
    def toZsh: String = s" $arg"
  }
  case class BinaryArg(switch: String, v: String) extends CmdLineArg {
    def toZsh: String = s" $switch $v" // No Single quoting etc for basic
  }

  /** What am I doing, no idea maybe making a Monoid or Semigroup, or maybe turn this into a map keys by -foo or --foo stuff
    * Note that this is a List not a Set so adding duplicates is allowed
    */
  case class Args(args: List[CmdLineArg]) {
    // Avoid pulling in optics in common
    def add(arg: CmdLineArg): Args      = this.copy(args = args.appended(arg))
    def add(arg: String): Args          = this.add(UnaryArg(arg))
    def add(k: String, v: String): Args = this.add(BinaryArg(k, v))
    def add(kv: (String, String)): Args = this.add(BinaryArg(kv._1, kv._2))
    def add(args: Args): Args           = Args(this.args ++ args.args)

    /** Ignores None */
    def add(oargs: Option[Args]): Args = oargs.map(add).getOrElse(this)

    // Short so who cares
    def raw: List[String] =
      this.args.flatMap {
        case UnaryArg(arg)        => arg :: Nil
        case BinaryArg(switch, v) => switch :: v :: Nil
      }

  }

  object Args {
    def empty: Args                       = Args(List.empty)
    implicit val argsMonoid: Monoid[Args] = new Monoid[Args] {
      def empty: Args                     = Args.empty
      def combine(x: Args, y: Args): Args = Args(x.args ::: y.args)
    }
  }

  @Lenses("_") case class Command(cmd: String, args: Args = Args.empty, cwd: Option[File] = None, xtraEnv: List[EnvVar] = List.empty) {
    // Avoid pulling in optics in common

    def raw: Seq[String]              = cmd :: args.raw
    def rawEnv: Seq[(String, String)] = this.xtraEnv.map(env => (env.name, env.value))

    override def toString = s"\nCMD: $cmd \nARGS: ${oprint(args)} \nENV ${oprint(xtraEnv)}"

    def toZsh: String =
      s"""
         |#!/usr/bin/env zsh
         |
         |# Echo Commands for Debugging / Entertainment
         |set -x
         |
         |# Environment Variables
         |${xtraEnv.map(_.toZsh).mkString("\n")}
         |
         |# Optionally move to a working directory
         |pushd ${cwd.map(_.getAbsolutePath).getOrElse(".")}  || exit
         |
         |$cmd \\
         |${args.args.map(a => " " + a.toZsh + " ").mkString("", "\\ \n", "\n")}
         |
         |popd || exit
         |
         |""".stripMargin
  }

  object Executor {

    /** Immutable copy of stdOut and stdErr */
    import sys.process._

    private def sb2optStr(sb: StringBuffer): Option[String] = optStr(sb.toString)

    private def optStr(s: String) =
      s match {
        case s if s.trim().isEmpty => None
        case s                     => Some(s)
      }

    /** This accumulates the logs and prints at the end. */
    private class LoggerState(realtimeToo: Boolean = true) {
      val newLine: String      = "\n"
      val stdErr: StringBuffer = new StringBuffer(10000) // Mutable
      val stdOut: StringBuffer = new StringBuffer(10000)
      val plog: ProcessLogger  =
        ProcessLogger(
          s => {
            printStdOut(s)
            stdOut.append(s).append(newLine)
          },
          s => {
            printStdErr(s)
            stdErr.append(s).append(newLine)
          }
        )

      def out: Option[String] = sb2optStr(stdOut)
      def err: Option[String] = sb2optStr(stdErr)

      private def printStdOut(s: String): Unit = if (realtimeToo) scribe.debug(s"STDOUT: $s")
      private def printStdErr(s: String): Unit = if (realtimeToo) scribe.warn(s"STDERR: $s")
    }

    /**
      * Executes a command line process, this is re-runnable.
      *
      * @param cmd
      *
      * @return Synchronous exit code and output with stdout and stderr. exit code 0 on success.
      *         If it cannot start the process, e.g. bad executable, it will raise an error
      */
    def run(cmd: Command): IO[CmdLineResult] = {
      for {
        state    <- IO(new LoggerState())
        process   = Process(cmd.raw, cmd.cwd, cmd.rawEnv: _*)
        exitCode <- IO(process.!(state.plog))
      } yield CmdLineResult(exitCode, state.out, state.err)
    }

    def responseError(rs: CmdLineResult): Option[Throwable] = {
      if (rs.exitCode != 0) OError(s"Exit Code Not Zero (${rs.exitCode}) : ${rs.stderr}").some
      else None
    }

  }

}
