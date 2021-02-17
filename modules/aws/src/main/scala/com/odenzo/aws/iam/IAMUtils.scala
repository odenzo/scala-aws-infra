package com.odenzo.aws.iam

import io.circe.Json
import io.circe.optics.JsonPath.root

trait IAMUtils {
  private val policyStatements = root.Statement.arr

  /** Combine the Statement object lists and returns new Json */
  def combinePolicies(a: Json, b: Json): Json = {
    val bObjs: Vector[Json] = policyStatements.getOption(b).getOrElse(Vector.empty[Json])
    policyStatements.modify(aObjs => bObjs.prependedAll(aObjs))(a)
  }

}

object IAMUtils extends IAMUtils
