package org.biobank.query.centres

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.domain.participants._
import org.biobank.query.db.DatabaseSchema
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
//import scalaz.Validation.FlatMap._
import scalaz._
import org.slf4j.LoggerFactory

@ImplementedBy(classOf[ShipmentSpecimensReadRepositorySlick])
trait ShipmentSpecimensReadRepository extends AsyncReadRepository[ShipmentSpecimenId, ShipmentSpecimen] {

  def forShipment(id: ShipmentId): Future[Seq[ShipmentSpecimen]]

  def forShipments(shipmentIds: ShipmentId*): Future[Seq[ShipmentSpecimen]]

  def countsForShipments(shipmentIds: ShipmentId*): Future[Map[ShipmentId, ShipmentSpecimenCounts]]

  def allForSpecimens(speciemnIds: SpecimenId*): Future[Seq[ShipmentSpecimen]]

  def getBySpecimen(shipmentId: ShipmentId, specimenId: SpecimenId): FutureValidation[ShipmentSpecimen]

  def getBySpecimens(
      shipmentId:  ShipmentId,
      specimenIds: SpecimenId*
    ): FutureValidation[Seq[ShipmentSpecimen]]

  def putAll(shipments: Seq[ShipmentSpecimen]): Future[Unit]

  def put(shipment: ShipmentSpecimen): Future[Unit]

  def remove(shipmentSpecimenId: ShipmentSpecimenId): Future[Unit]

  def removeAll(): Future[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class ShipmentSpecimensReadRepositorySlick @Inject()(
    protected val dbConfigProvider: DatabaseConfigProvider
  )(
    implicit
    val ec: ExecutionContext)
    extends ShipmentSpecimensReadRepository with DatabaseSchema {
  import dbConfig.profile.api._
  import org.biobank.CommonValidations._

  protected val log = LoggerFactory.getLogger(this.getClass)

  protected def notFound(id: ShipmentSpecimenId): IdNotFound = IdNotFound(s"shipment specimen id: $id")

  def exists(predicate: ShipmentSpecimen => Boolean): Future[Boolean] = ???

  def getKeys: scala.concurrent.Future[Iterable[ShipmentSpecimenId]] = ???

  def getValues: scala.concurrent.Future[Iterable[ShipmentSpecimen]] = ???

  def isEmpty: scala.concurrent.Future[Boolean] = ???

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getByKey(shipmentSpecimenId: ShipmentSpecimenId): FutureValidation[ShipmentSpecimen] = {
    FutureValidation(
      db.run(shipmentSpecimens.filter(ss => ss.id === shipmentSpecimenId).result.headOption)
        .map(_.toSuccessNel(notFound(shipmentSpecimenId).toString))
    )
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def forShipment(id: ShipmentId): Future[Seq[ShipmentSpecimen]] = {
    db.run(shipmentSpecimens.filter(ss => ss.shipmentId === id).result)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def forShipments(shipmentIds: ShipmentId*): Future[Seq[ShipmentSpecimen]] = {
    val query = shipmentSpecimens.filter(ss => ss.shipmentId.inSet(shipmentIds))
    db.run(query.result)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def countsForShipments(shipmentIds: ShipmentId*): Future[Map[ShipmentId, ShipmentSpecimenCounts]] = {
    val query = shipmentSpecimens
      .filter(ss => ss.shipmentId.inSet(shipmentIds))
      .groupBy(_.shipmentId)
      .map {
        case (shipmentId, ss) =>
          (shipmentId, ss.length, ss.map(_.state === ShipmentItemState.Present).length)
      }

    db.run(query.result.map {
      _.map {
        case (shipmentId, specimenCount, presentSpecimenCount) =>
          (shipmentId -> ShipmentSpecimenCounts(specimenCount, presentSpecimenCount))
      }.toMap
    })
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def allForSpecimens(specimenIds: SpecimenId*): Future[Seq[ShipmentSpecimen]] = {
    val query = shipmentSpecimens.filter(ss => ss.specimenId.inSet(specimenIds))
    db.run(query.result)
  }

  def getBySpecimen(shipmentId: ShipmentId, specimenId: SpecimenId): FutureValidation[ShipmentSpecimen] = {
    ???
  }

  def getBySpecimens(
      shipmentId:  ShipmentId,
      specimenIds: SpecimenId*
    ): FutureValidation[Seq[ShipmentSpecimen]] = {
    ???
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def putAll(shipmentSpecimensToAdd: Seq[ShipmentSpecimen]): Future[Unit] = {
    db.run(shipmentSpecimens ++= shipmentSpecimensToAdd).map(_ => ())
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def put(shipmentSpecimen: ShipmentSpecimen): Future[Unit] = {
    db.run(shipmentSpecimens.insertOrUpdate(shipmentSpecimen).map(_ => ()))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def remove(shipmentSpecimenId: ShipmentSpecimenId): Future[Unit] = {
    val query  = shipmentSpecimens.filter(s => s.id === shipmentSpecimenId)
    val action = query.delete
    db.run(action.map(_ => ()))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def removeAll(): Future[Unit] = db.run(shipmentSpecimens.delete.map(_ => ()))
}
