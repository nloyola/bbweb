package org.biobank.fixtures

import org.biobank.controllers.CacheForTesting
import org.biobank.domain.Factory
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import play.api.cache.{AsyncCacheApi, DefaultSyncCacheApi, SyncCacheApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder

trait TestFixture extends AnyFunSpec with BeforeAndAfterEach {

  val app = new GuiceApplicationBuilder()
    .overrides(bind[SyncCacheApi].to[DefaultSyncCacheApi])
    .overrides(bind[AsyncCacheApi].to[CacheForTesting])
    .build

  protected val factory = new Factory

}
