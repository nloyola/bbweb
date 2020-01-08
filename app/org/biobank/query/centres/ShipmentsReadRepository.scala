package org.biobank.query.centres

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.query.db.DatabaseSchema
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
//import scalaz.Validation.FlatMap._
import scalaz._
import org.slf4j.LoggerFactory

@ImplementedBy(classOf[ShipmentsReadRepositorySlick])
trait ShipmentsReadRepository extends AsyncReadRepository[ShipmentId, Shipment] {

  def withCentres(centreIds: Set[CentreId]): Future[Seq[Shipment]]

  def getCreated(id: ShipmentId): FutureValidation[CreatedShipment]

  def getUnpacked(id: ShipmentId): FutureValidation[UnpackedShipment]

  def putAll(shipments: Seq[Shipment]): Future[Unit]

  def put(shipment: Shipment): Future[Unit]

  def remove(shipmentId: ShipmentId): Future[Unit]

  def removeAll(): Future[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
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

  def exists(predicate: Shipment => Boolean): Future[Boolean] = ???

  def getKeys: scala.concurrent.Future[Iterable[org.biobank.domain.centres.ShipmentId]] = ???

  def getValues: scala.concurrent.Future[Iterable[org.biobank.domain.centres.Shipment]] = ???

  def isEmpty: scala.concurrent.Future[Boolean] = ???

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getByKey(shipmentId: ShipmentId): FutureValidation[Shipment] = {
    FutureValidation(
    db.run(shipments.filter(s => s.id === shipmentId).result.headOption)
      .map(_.toSuccessNel(notFound(shipmentId).toString))
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def withCentres(centreIds: Set[CentreId]): Future[Seq[Shipment]] = {
    val query = shipments
      .filter(s => s.originCentreId.inSet(centreIds) || s.destinationCentreId.inSet(centreIds))
    db.run(query.result)
  }

  def getCreated(id: ShipmentId): FutureValidation[CreatedShipment] = ???

  def getUnpacked(id: ShipmentId): FutureValidation[UnpackedShipment] = ???

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def putAll(shipmentsToAdd: Seq[Shipment]): Future[Unit] = {
    db.run(shipments ++= shipmentsToAdd).map(_ => ())
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def put(shipment: Shipment): Future[Unit] = {
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
}
