package com.odenzo.utils

/** DNS Names, this is actually a DNS Subdomain now.
  * Used alot in K8S, and have regular expression that must match.
  * This has utilities to help and also to wrap.
  * Default constructor DOES NOT validate or mutate/normalize.
  */
case class DnsName(names: String)

object DnsName {

  // '[a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*')

  def normalize(s: String): String = {
    s.toLowerCase.replace('_', '-')
  }
}
