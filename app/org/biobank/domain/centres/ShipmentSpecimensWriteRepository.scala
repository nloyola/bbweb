package org.biobank.domain.centres

import cats.data.Validated
import cats.implicits._
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.participants.{Specimen, SpecimenId}
import org.biobank.validation.Validation._

@ImplementedBy(classOf[ShipmentSpecimensWriteRepositoryImpl])
trait ShipmentSpecimensWriteRepository extends CatsReadWriteRepository[ShipmentSpecimenId, ShipmentSpecimen] {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def exists(id: ShipmentSpecimenId): ValidationResult[ShipmentSpecimen]

  def forShipment(id: ShipmentId): Set[ShipmentSpecimen]

  def allForSpecimen(id: SpecimenId): Set[ShipmentSpecimen]

  def getBySpecimen(shipmentId: ShipmentId, specimen: Specimen): ValidationResult[ShipmentSpecimen]

  def getBySpecimens(shipmentId: ShipmentId, specimens: Specimen*): ValidationResult[List[ShipmentSpecimen]]

  def shipmentSpecimenCount(shipmentId: ShipmentId, state: EntityState): Int
}

@Singleton
class ShipmentSpecimensWriteRepositoryImpl @Inject()(val testData: TestData)
    extends StmCatsReadWriteRepositoryImpl[ShipmentSpecimenId, ShipmentSpecimen](v => v.id)
    with ShipmentSpecimensWriteRepository {

  override def init(): Unit = {
    super.init()
    testData.testShipmentSpecimens.foreach(put)
  }

  def nextIdentity(): ShipmentSpecimenId = new ShipmentSpecimenId(nextIdentityAsString)

  protected def notFound(id: ShipmentSpecimenId): IdNotFound = IdNotFound(s"shipment specimen id: $id")

  protected def shipmentAndSpecimenNotFound(shipmentId: ShipmentId, specimenId: SpecimenId) =
    IdNotFound(s"shipment id: $shipmentId, specimen id: $specimenId")

  protected def specimenNotFound(specimen: Specimen) =
    IdNotFound(s"shipment specimen with inventory ID: ${specimen.inventoryId}").invalidNec

  def exists(id: ShipmentSpecimenId): ValidationResult[ShipmentSpecimen] = {
    Validated.fromOption(getByKey(id), notFound(id)).toValidatedNec
  }

  def forShipment(id: ShipmentId): Set[ShipmentSpecimen] =
    getValues.filter { ss =>
      ss.shipmentId == id
    }.toSet

  def allForSpecimen(id: SpecimenId): Set[ShipmentSpecimen] =
    getValues.filter { ss =>
      ss.specimenId == id
    }.toSet

  def getBySpecimen(shipmentId: ShipmentId, specimen: Specimen): ValidationResult[ShipmentSpecimen] =
    getValues.find(ss => (ss.shipmentId == shipmentId) && (ss.specimenId == specimen.id)) match {
      case None     => specimenNotFound(specimen)
      case Some(ss) => ss.validNec
    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getBySpecimens(shipmentId: ShipmentId, specimens: Specimen*): ValidationResult[List[ShipmentSpecimen]] =
    specimens.map(getBySpecimen(shipmentId, _)).toList.sequence

  def shipmentSpecimenCount(shipmentId: ShipmentId, state: EntityState): Int =
    forShipment(shipmentId).filter(ss => ss.state == state).size

}
