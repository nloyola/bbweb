package org.biobank.query.centres

import akka.NotUsed
import akka.actor._
import akka.stream.scaladsl.Source
import akka.persistence.inmemory.query.scaladsl.InMemoryReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import javax.inject.Inject
import org.biobank._
import org.biobank.domain.centres._
import org.biobank.query.db._
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class ShipmentsQueryInMemory @Inject()(
    private val testData:                      TestData,
    protected val dbConfigProvider:            DatabaseConfigProvider,
    protected val centreRepository:            CentreRepository,
    protected val shipmentsRepository:         ShipmentsReadRepository,
    protected val shipmentSpecimensRepository: ShipmentSpecimensReadRepository,
    protected val sequenceNumbersDao:          SequenceNumbersDao
  )(
    implicit
    val system: ActorSystem,
    val ec:     ExecutionContext)
    extends ShipmentsQuery {

  def eventsByPersistenceId(
      persistenceId:  String,
      fromSequenceNr: Long,
      toSequenceNr:   Long
    ): Source[EventEnvelope, NotUsed] = {
    PersistenceQuery(system)
      .readJournalFor[InMemoryReadJournal](InMemoryReadJournal.Identifier).eventsByPersistenceId(
        persistenceId,
        fromSequenceNr,
        toSequenceNr
      )
  }

}
