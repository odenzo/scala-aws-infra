package com.odenzo.utils

import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.utils.OPrint.oprint
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import io.circe
import io.circe.config.syntax._
import io.circe.{Decoder, Json}

/** Global HOCON Configuration */
object AppConfig {

  def config: IO[Config] = IO(ConfigFactory.load())

  def metadata: IO[Map[String, String]] = {
    for {
      c  <- config
      md <- IO.fromEither(c.as[Map[String, String]]("metadata").leftMap(new Exception("Error Parsing Config MetaData", _)))
    } yield md
  }

  def dumpConfigAtPath(path: String, c: Config): Unit = {
    for {
      json <- configAsJson(c.getConfig(path))
      str   = json.spaces4SortKeys
      _     = scribe.info(s"Config at $path is\n $str")
    } yield str
  }

  def configAsJson(c: Config): IO[Json] = {
    val concise = c.root().render(ConfigRenderOptions.concise())
    IO.fromEither(circe.parser.parse(concise))
  }

  /** Helper to decode a config (with logging) at absolute path */
  def decodeConfigAtPath[T: Decoder](path: String, conf: Option[Config] = None): IO[T] = {
    for {
      base <- conf.fold(AppConfig.config)(IO(_))
      _     = AppConfig.dumpConfigAtPath(path, base)
      vt   <- IO.fromEither(base.getConfig(path).as[T])
      _     = scribe.info(s"Config @ $path:\n" + oprint(vt))
    } yield vt
  }

  def unsafeDumpConfig(atPath: Option[String]): Unit = {
    val config           = ConfigFactory.load()
    val jsonIO: IO[Json] = atPath match {
      case Some(path) =>
        val sub = config.withOnlyPath(path)
        if (sub.isEmpty) {
          scribe.error(s"Path $path is EMPTY!!")
          configAsJson(config)
        } else {
          configAsJson(sub)
        }
      case None       => configAsJson(config)

    }
    val json             = jsonIO.unsafeRunSync()
    scribe.info(s"JSON: ${json.spaces4SortKeys}")

  }

}
