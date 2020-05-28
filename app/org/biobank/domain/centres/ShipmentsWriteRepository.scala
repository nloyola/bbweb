package org.biobank.domain.centres

import cats.data.Validated
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.slf4j.{Logger, LoggerFactory}
import org.biobank.validation.Validation._

@ImplementedBy(classOf[ShipmentWriteRepositoryStm])
trait ShipmentsWriteRepository extends CatsReadWriteRepository[ShipmentId, Shipment] {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def exists(id: ShipmentId): ValidationResult[Shipment]

  /**
   * Returns all shipments either being sent to or being received at the centre with centreId.
   */
  def withCentre(centreId: CentreId): Set[Shipment]

  def getCreated(id: ShipmentId): ValidationResult[CreatedShipment]

  def getUnpacked(id: ShipmentId): ValidationResult[UnpackedShipment]

}

@Singleton
class ShipmentWriteRepositoryStm @Inject()(val testData: TestData)
    extends StmCatsReadWriteRepositoryImpl[ShipmentId, Shipment](v => v.id) with ShipmentsWriteRepository {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  override def init(): Unit = {
    super.init()
    testData.testShipments.foreach(put)
  }

  def nextIdentity(): ShipmentId = new ShipmentId(nextIdentityAsString)

  protected def notFound(id: ShipmentId): IdNotFound = IdNotFound(s"shipment id: $id")

  def withCentre(centreId: CentreId): Set[Shipment] =
    getValues.filter { s =>
      (s.originCentreId == centreId) || (s.destinationCentreId == centreId)
    }.toSet

  def exists(id: ShipmentId): ValidationResult[Shipment] = {
    Validated
      .fromOption(getByKey(id), IdNotFound(s"shipment with id not found: $id")).toValidatedNec
  }

  def getCreated(id: ShipmentId): ValidationResult[CreatedShipment] = {
    exists(id).andThen(_.isCreated)
  }

  def getUnpacked(id: ShipmentId): ValidationResult[UnpackedShipment] = {
    exists(id).andThen(_.isUnpacked)
  }

}
