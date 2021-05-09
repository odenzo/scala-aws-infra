package com.odenzo.utils

import cats._
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.utils.CommandLineArgs.Args
import com.odenzo.utils.OPrint.oprint
import monocle.macros.Lenses
import os.Shellable

import java.io.File

object CommandLineArgs {

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

    def raw: List[String] =
      this.args.flatMap {
        case UnaryArg(arg)        => arg :: Nil
        case BinaryArg(switch, v) => switch :: v :: Nil
      }

    /** for os.proc usage. This assumes the actuall command in addition to args */
    def toShellable(prepend: List[String]): Shellable = {
      Shellable(prepend ::: raw)
    }
  }

  object Args {
    def empty: Args                       = Args(List.empty)
    implicit val argsMonoid: Monoid[Args] = new Monoid[Args] {
      def empty: Args                     = Args.empty
      def combine(x: Args, y: Args): Args = Args(x.args ::: y.args)
    }
  }

}
