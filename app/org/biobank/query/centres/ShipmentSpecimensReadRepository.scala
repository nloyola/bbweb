package org.biobank.query.centres

import cats.data._
import cats.implicits._
import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank.domain._
import org.biobank.domain.centres._
import org.biobank.domain.participants._
import org.biobank.dto.centres.ShipmentSpecimenDto
import org.biobank.query.db.DatabaseSchema
import org.biobank.validation.Validation._
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import org.slf4j.LoggerFactory

@ImplementedBy(classOf[ShipmentSpecimensReadRepositorySlick])
trait ShipmentSpecimensReadRepository
    extends CatsAsyncReadRepository[ShipmentSpecimenId, ShipmentSpecimenDto] {

  def forShipment(id: ShipmentId): Future[Seq[ShipmentSpecimenDto]]

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def forShipment(
      shipmentId:          ShipmentId,
      shipmentSpecimenIds: ShipmentSpecimenId*
    ): Future[Seq[ShipmentSpecimenDto]]

  def forShipments(shipmentIds: ShipmentId*): Future[Seq[ShipmentSpecimenDto]]

  def countsForShipments(shipmentIds: ShipmentId*): Future[Map[ShipmentId, ShipmentSpecimenCounts]]

  def allForSpecimens(speciemnIds: SpecimenId*): Future[Seq[ShipmentSpecimenDto]]

  def getBySpecimen(shipmentId: ShipmentId, specimenId: SpecimenId): Option[ShipmentSpecimenDto]

  def getBySpecimens(shipmentId: ShipmentId, specimenIds: SpecimenId*): Seq[ShipmentSpecimenDto]

  def putAll(shipments: Seq[ShipmentSpecimenDto]): Future[Unit]

  def put(shipment: ShipmentSpecimenDto): Future[Unit]

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

  protected val log = LoggerFactory.getLogger(this.getClass)

  protected def notFound(id: ShipmentSpecimenId): IdNotFound = IdNotFound(s"shipment specimen id: $id")

  def exists(predicate: ShipmentSpecimenDto => Boolean): Future[Boolean] = ???

  def getKeys: scala.concurrent.Future[Iterable[ShipmentSpecimenId]] = ???

  def getValues: scala.concurrent.Future[Iterable[ShipmentSpecimenDto]] = ???

  def isEmpty: scala.concurrent.Future[Boolean] = ???

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getByKey(id: ShipmentSpecimenId): FutureValidationResult[ShipmentSpecimenDto] = {
    EitherT(db.run(shipmentSpecimens.filter(ss => ss.id === id).result.headOption).map {
      _ match {
        case Some(ss) => Either.right(ss)
        case None     => Either.left(NonEmptyChain(notFound(id)))
      }
    })
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def forShipment(id: ShipmentId): Future[Seq[ShipmentSpecimenDto]] = {
    db.run(shipmentSpecimens.filter(ss => ss.shipmentId === id).result)
  }

  //@SuppressWarnings(Array("org.wartremover.warts.Any"))
  def forShipment(
      shipmentId:          ShipmentId,
      shipmentSpecimenIds: ShipmentSpecimenId*
    ): Future[Seq[ShipmentSpecimenDto]] = {
    forShipment(shipmentId).map(_.filter(ss => shipmentSpecimenIds.contains(ss.id)))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def forShipments(shipmentIds: ShipmentId*): Future[Seq[ShipmentSpecimenDto]] = {
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
          (shipmentId, ss.length, ss.map(_.state === ShipmentSpecimen.presentState).length)
      }

    db.run(query.result.map {
      _.map {
        case (shipmentId, specimenCount, presentSpecimenCount) =>
          (shipmentId -> ShipmentSpecimenCounts(specimenCount, presentSpecimenCount))
      }.toMap
    })
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def allForSpecimens(specimenIds: SpecimenId*): Future[Seq[ShipmentSpecimenDto]] = {
    val query = shipmentSpecimens.filter(ss => ss.specimenId.inSet(specimenIds))
    db.run(query.result)
  }

  def getBySpecimen(shipmentId: ShipmentId, specimenId: SpecimenId): Option[ShipmentSpecimenDto] = {
    ???
  }

  def getBySpecimens(shipmentId: ShipmentId, specimenIds: SpecimenId*): Seq[ShipmentSpecimenDto] = {
    ???
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def putAll(shipmentSpecimensToAdd: Seq[ShipmentSpecimenDto]): Future[Unit] = {
    db.run(DBIO.sequence(shipmentSpecimensToAdd.map(shipmentSpecimens.insertOrUpdate(_)))).map(_ => ())
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def put(shipmentSpecimen: ShipmentSpecimenDto): Future[Unit] = {
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

  def init(): Future[Unit] = {
    db.run(shipmentSpecimens.schema.createIfNotExists)
  }
}
