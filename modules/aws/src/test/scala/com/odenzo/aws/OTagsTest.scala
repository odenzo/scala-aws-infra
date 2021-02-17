package com.odenzo.aws

import com.odenzo.aws.testutils.AWSBaseTest

class OTagsTest extends AWSBaseTest {

  test("Tag Concat") {
    val first = OTags.from("a" -> "One")
    val sec   = OTags.from("a" -> "Two")
    val con   = first.withTags(sec)
    scribe.debug(s"Con: ${con}")
    con.tags.get("a") mustBe "Two"
  }
}
