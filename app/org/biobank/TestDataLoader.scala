package org.biobank

import javax.inject.{Inject, Singleton}
import org.biobank.query.db.DatabaseSchema
import play.api.db.slick.DatabaseConfigProvider
//import play.api.Logger
//import scala.concurrent.{ExecutionContext, Future}
//import scala.util.{Failure, Success}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.ImplicitParameter"))
@Singleton
class TestDataLoader @Inject()(
    private val testData:           TestData,
    protected val dbConfigProvider: DatabaseConfigProvider)
//(implicit val ec: ExecutionContext)
    extends DatabaseSchema {

  // import dbConfig.profile.api._

  // private val log: Logger = Logger(this.getClass)

  //   FIXME: not required? See implementation of ShipmentsQuery
  // private def loadShipments(): Future[Unit] = {
  //   val setup = DBIO.seq(shipments.delete, shipments ++= testData.testShipments.toSeq)

  //   db.run(setup).andThen {
  //     case Success(_) => log.info("Initial data inserted")
  //     case Failure(e) => log.error(s"Initial data not inserted: ${e.getMessage}")
  //   }
  // }

  // loadShipments
}
