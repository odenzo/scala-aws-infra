package com.odenzo.aws.kafka

import cats.effect.IO
import com.odenzo.aws.testutils.AWSBaseTest
import com.odenzo.utils.OPrint.oprint

class KafkaTest extends AWSBaseTest {

  test("Get Brokers") {
    val res: IO[(Array[String], Array[String])] = for {
      rs        <- Kafka.getBootstrapBrokers("arn:aws:kafka:us-east-1:879130378853:cluster/horn-k8s-RC2/51f26889-c0f7-44b1-b409-6acf4958d8c9-6")
      brokers    = rs.bootstrapBrokerString.split(',')
      brokersTls = rs.bootstrapBrokerStringTls.split(',')
    } yield (brokers, brokersTls)
    val answer                                  = res.unsafeRunSync()
    scribe.info(s"${oprint(answer)}")
  }
}
