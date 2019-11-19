package org.biobank.modules

import com.google.inject.AbstractModule
import org.biobank.{Global, TestData, TestDataLoader}
import org.biobank.query.centres.ShipmentsQuery

class BbwebModule extends AbstractModule {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  override def configure() = {
    bind(classOf[Global]).asEagerSingleton
    bind(classOf[TestData]).asEagerSingleton
    bind(classOf[TestDataLoader]).asEagerSingleton
    bind(classOf[ShipmentsQuery]).asEagerSingleton
  }

}
