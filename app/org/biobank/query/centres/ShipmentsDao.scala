package org.biobank.query.centres

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.query.db.DatabaseSchema
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
//import scalaz.Validation.FlatMap._
import scalaz._
import org.slf4j.LoggerFactory

@ImplementedBy(classOf[ShipmentsDaoSlick])
trait ShipmentsDao {

  def addAll(shipments: Seq[Shipment]): Future[Unit]

  def insertOrUpdate(shipment: Shipment): Future[Unit]

  def remove(shipmentId: ShipmentId): Future[Unit]

  def shipmentWithId(shipmentId: ShipmentId): Future[DomainValidation[Shipment]]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class ShipmentsDaoSlick @Inject()(
    protected val dbConfigProvider: DatabaseConfigProvider
  )(
    implicit
    val ec: ExecutionContext)
    extends ShipmentsDao with DatabaseSchema {
  import dbConfig.profile.api._

  protected val log = LoggerFactory.getLogger(this.getClass)

  def addAll(shipmentsToAdd: Seq[Shipment]): Future[Unit] = {
    db.run(shipments ++= shipmentsToAdd).map(_ => ())
  }

  def insertOrUpdate(shipment: Shipment): Future[Unit] = {
    db.run(shipments.insertOrUpdate(shipment).map(_ => ()))
  }

  def remove(shipmentId: ShipmentId): Future[Unit] = {
    val query  = shipments.filter(s => s.id === shipmentId)
    val action = query.delete
    db.run(action.map(_ => ()))
  }

  def shipmentWithId(shipmentId: ShipmentId): Future[DomainValidation[Shipment]] = {
    db.run(shipments.filter(s => s.id === shipmentId).result.headOption)
      .map(_.toSuccessNel(s"shipment not found: $shipmentId"))
  }

}
