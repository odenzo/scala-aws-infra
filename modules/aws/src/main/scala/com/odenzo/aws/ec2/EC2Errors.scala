package com.odenzo.aws.ec2

object EC2Errors {

  def unknownError(s: String = "No Further Info") = new Throwable(s"resultStatus false doing: $s")

}
