package org.biobank.domain

import org.scalatest.funspec.AnyFunSpec

trait DomainSpec extends AnyFunSpec {

  // TODO: can this be replaced with an Injectable singleton?
  val factory = new Factory

}
