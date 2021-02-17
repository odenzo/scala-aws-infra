package com.odenzo.utils

import cats.effect._

import java.io.{File, FileInputStream, FileOutputStream}
import scala.io.{Codec, Source}

object FileIO {

  def readUtfTextFile(f: File): IO[String] = {
    inputStream(f).use { fis =>
      IO(Source.fromInputStream(fis)(Codec.UTF8).getLines().mkString("\n"))
    }
  }

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f)) // build
    } { inStream =>
      IO(inStream.close()).handleErrorWith(_ => IO.unit) // release
    }

  /** Resource Manaaged (File) OutputStream */
  def outputStream(f: File): Resource[IO, FileOutputStream] = {
    Resource.make {
      IO(new FileOutputStream(f))
    } { outStream =>
      IO(outStream.close()).handleErrorWith(_ => IO.unit) // release
    }
  }
}
