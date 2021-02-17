package com.odenzo.aws.redis

import cats._
import cats.data._
import cats.syntax.all._
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.IOU
import com.odenzo.utils.OPrint.oprint

import scala.jdk.CollectionConverters._

class RedisTest extends AWSBaseTest {

  test("Getting Endpoint") {

    val prog =
      for {
        cluster   <- Redis.findClusterByName("eek6") >>= IOU.required("Cache Cluster")
        numCache   = cluster.numCacheNodes()
        cachenodes = cluster.cacheNodes().asScala.toList
      } yield (numCache, cachenodes)

    val res = prog.attempt.unsafeRunSync()
    scribe.debug(s"Endpoint ${oprint(res)}")

  }

  test("Get Endpoings") {
    val res = Redis.getEndpointsByName("dev").unsafeRunSync()
    scribe.info(s"Endpoint ${oprint(res)}")
  }
  test("Get Tags") {
    val prog = for {
      cluster <- Redis.findClusterByName("eek6") >>= IOU.required("Cache Cluster")
      tags    <- Redis.findTagsForCacheCluster(cluster.arn())
    } yield tags

    val res = prog.unsafeRunSync()
    scribe.info(s"Endpoint ${oprint(res)}")
  }

  test("ReplicationGroup List") {
    val prog =
      for {
        stream <- Redis.listReplicationGroups()
        res    <- stream.compile.toList
      } yield (res)

    val res = prog.attempt.unsafeRunSync()
    scribe.debug(s"Endpoint ${oprint(res)}")

  }
}
