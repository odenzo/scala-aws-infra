package com.odenzo.utils

import cats.effect._

import java.io.{File, FileInputStream, FileOutputStream}
import scala.io.{Codec, Source}

/** This would seem like something good to make F[_]:Async */
object FileIO {

  def readUtfTextFile(f: File): SyncIO[String] = {
    inputStream(f).use { fis =>
      SyncIO.delay(Source.fromInputStream(fis)(Codec.UTF8).getLines().mkString("\n"))
    }
  }

  def inputStream(f: File): Resource[SyncIO, FileInputStream] =
    Resource.make {
      SyncIO.delay(new FileInputStream(f)) // build
    } { inStream =>
      SyncIO.delay(inStream.close()).handleErrorWith(_ => SyncIO.unit) // release
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
