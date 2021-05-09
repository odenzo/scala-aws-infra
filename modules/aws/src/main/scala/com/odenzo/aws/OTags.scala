package com.odenzo.aws

import cats.syntax.all._
import io.circe.JsonObject
import io.circe.syntax.EncoderOps

import java.util
import scala.jdk.CollectionConverters._

// Monoid for this
case class OTag(v: (String, String)) {
  def name: String    = v._1
  def content: String = v._2
}

object OTag {
  def apply(name: String, content: String): OTag = OTag((name, content))
  def from(name: String, content: String): OTag  = OTag((name, content))
}

/** Time to rewrite this too - tags for labeling things in AWS, Yaml, Kubernetes each with their own quirks
  * SemiGroup for this.
  * Seperate those out as validators and "fixers"  for the different use-cases
  */
case class OTags(tags: Map[String, String]) {
  def withTag(t: (String, String)): OTags     = OTags(tags + ((t._1, t._2)))
  def withTags(t: (String, String)*): OTags   = OTags(this.tags.concat(t.toMap))
  def withTag(t: OTag): OTags                 = withTag(t.v)
  def withoutTag(key: String): OTags          = OTags(tags - key)
  def withTags(o: OTags): OTags               = OTags(this.tags.concat(o.tags)) // Cat combine will do too
  def withName(s: String): OTags              = OTags(this.tags.updated("Name", s))
  def modifyName(fn: String => String): OTags = withName(fn(this.tags.getOrElse("Name", "")))
  def contains(t: OTag): Boolean              = tags.get(t.name).exists(_ === t.content)
  def getName: Option[String]                 = this.tags.get("Name")

  /** Convertor to AWS subpackage tag type */
  def via[T](fn: (String, String) => T): util.Collection[T] = OTags.toPackageTags(this, fn)

  def makeKubernetesLabelSafe: OTags = {
    // Where or where are you? '([A-Za-z0-9][-A-Za-z0-9_.]*)?[A-Za-z0-9]'
    val nt = this.tags.iterator.map { case (k, v) =>
      (k.replace('/', '-'), v)
    }
    OTags(nt.toMap)
  }

  def toJsonObject: JsonObject = tags.asJsonObject
}

/** Seems each AWS sub-library (e.g. ecs/rds) has their own Tag model object with no way to move
  * between them easily. So, we store indepedant format and use a supplied function to
  * create ecs.model.Tag or rds.model.Tag e.g.
  */
object OTags {
  val empty: OTags                      = OTags(Map.empty)
  def from(t: (String, String)*): OTags = OTags.empty.withTags(t: _*)

  /** Functions converts generic tag to package specific using the supplied function.
    * The resulting Java collection can be passed into builder.tags() AWS methods.
    */
  def toPackageTags[T](ot: OTags, fn: (String, String) => T): util.Collection[T] = {
    ot.tags.iterator.map(fn.tupled).toList.asJavaCollection
  }

}
