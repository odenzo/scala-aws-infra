package com.odenzo.utils

import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import com.odenzo.utils.CommandLine._

import java.io.File

class CommandLineTest extends BaseTest {

  test("Display of Command") {
    val cmd = Command(
      "ls",
      Args.empty.add("-lrt").add("/tmp").add("/usr/local"),
      new File("/Users/home/run").some,
      List(EnvVar("PATH", "/to/nowhere"), EnvVar("PASSWRD", "bubba"))
    )
    scribe.debug(s"$cmd")

    scribe.info(s"ZSH Version: \n ${cmd.toZsh}")
  }

  test("Execute Simple ReRun") {
    val c                      = Command("date", Args.empty)
    val res: IO[CmdLineResult] = Executor.run(c)
    val r1                     = res.unsafeRunSync()

    scribe.debug(s"Date: ${r1.exitCode} -- ${r1.stdout} -- ${r1.stderr}")
    Thread.sleep(1000)
    scribe.debug(s"Date: ${res.unsafeRunSync()}")
  }

  test("Error Code") {
    assertThrows[java.io.IOException] {
      Executor.run(Command("/never/found")).unsafeRunSync()
    }
  }
  test("Exit Code Bads") {
    val cl  = Command("cat", Args.empty.add("/never/found"))
    val res = Executor.run(cl).unsafeRunSync()
    scribe.debug(s"$res")
    res.stdout mustBe empty
    res.stderr mustBe defined
    res.exitCode mustNot equal(0)
  }

}
