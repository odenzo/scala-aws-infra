package com.odenzo.utils

import io.circe
import io.circe.generic.auto._

case class AppMetaData(app_name: String, version: String, xtra: Map[String, String])

object AppMetaData {

  // With k8s the config file may be changed while running.
  def metaData: Either[circe.Error, AppMetaData] = io.circe.config.parser.decodePath[AppMetaData]("metadata")

}
