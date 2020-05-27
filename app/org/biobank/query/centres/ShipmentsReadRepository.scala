package org.biobank.query.centres

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.dto.centres.ShipmentDto
import org.biobank.query.db.DatabaseSchema
import play.api.db.slick.DatabaseConfigProvider
import scalaz.Scalaz._
// import scalaz.Validation.FlatMap._
import org.slf4j.LoggerFactory
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[ShipmentsReadRepositorySlick])
trait ShipmentsReadRepository extends AsyncReadRepository[ShipmentId, ShipmentDto] {

  def withCentres(centreIds: Set[CentreId]): Future[Seq[ShipmentDto]]

  def getCreated(id: ShipmentId): FutureValidation[ShipmentDto]

  def getUnpacked(id: ShipmentId): FutureValidation[ShipmentDto]

  def putAll(shipments: Seq[ShipmentDto]): Future[Unit]

  def put(shipment: ShipmentDto): Future[Unit]

  def remove(shipmentId: ShipmentId): Future[Unit]

  def removeAll(): Future[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements"))
class ShipmentsReadRepositorySlick @Inject()(
    protected val dbConfigProvider: DatabaseConfigProvider
  )(
    implicit
    val ec: ExecutionContext)
    extends ShipmentsReadRepository with DatabaseSchema {
  import dbConfig.profile.api._
  import org.biobank.CommonValidations._

  protected val log = LoggerFactory.getLogger(this.getClass)

  protected def notFound(id: ShipmentId): IdNotFound = IdNotFound(s"shipment id: $id")

  def exists(predicate: ShipmentDto => Boolean): Future[Boolean] = ???

  def getKeys: Future[Iterable[ShipmentId]] = ???

  def getValues: Future[Iterable[ShipmentDto]] = ???

  def isEmpty: Future[Boolean] = ???

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getByKey(shipmentId: ShipmentId): FutureValidation[ShipmentDto] = {
    FutureValidation(
      db.run(shipments.filter(s => s.id === shipmentId).result.headOption)
        .map(_.toSuccessNel(notFound(shipmentId).toString))
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def withCentres(centreIds: Set[CentreId]): Future[Seq[ShipmentDto]] = {
    val query = shipments
      .filter(s => s.originCentreId.inSet(centreIds) || s.destinationCentreId.inSet(centreIds))
    db.run(query.result)
  }

  def getCreated(id: ShipmentId): FutureValidation[ShipmentDto] = ???

  def getUnpacked(id: ShipmentId): FutureValidation[ShipmentDto] = ???

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def putAll(shipmentsToAdd: Seq[ShipmentDto]): Future[Unit] = {
    db.run(DBIO.sequence(shipmentsToAdd.map(shipments.insertOrUpdate(_)))).map(_ => ())
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def put(shipment: ShipmentDto): Future[Unit] = {
    db.run(shipments.insertOrUpdate(shipment).map(_ => ()))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def remove(shipmentId: ShipmentId): Future[Unit] = {
    val query  = shipments.filter(s => s.id === shipmentId)
    val action = query.delete
    db.run(action.map(_ => ()))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def removeAll(): Future[Unit] = db.run(shipments.delete.map(_ => ()))

  def init(): Future[Unit] = {
    db.run(shipments.schema.createIfNotExists)
  }
}
