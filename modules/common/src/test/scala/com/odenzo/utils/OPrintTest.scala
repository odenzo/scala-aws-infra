package com.odenzo.utils

import com.odenzo.utils.OPrint.oprint

class OPrintTest extends BaseTest {

  case class FooBar(x: Int, s: String, e: Throwable, sshs: Secret)
  val t = FooBar(12, "hello momma", OError("Bad Rabbit"), Secret("shazam"))
  test("pprint") {
    scribe.info(s"${oprint(t)}")
    scribe.info(s"${oprint(t)}")
  }

}
