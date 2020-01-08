package org.biobank.domain.centres

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.centres.ShipmentItemState._
import org.biobank.domain.participants.{Specimen, SpecimenId}
import scalaz.Scalaz._

@ImplementedBy(classOf[ShipmentSpecimensWriteRepositoryImpl])
trait ShipmentSpecimensWriteRepository extends ReadWriteRepository[ShipmentSpecimenId, ShipmentSpecimen] {

  def forShipment(id: ShipmentId): Set[ShipmentSpecimen]

  def allForSpecimen(id: SpecimenId): Set[ShipmentSpecimen]

  def getBySpecimen(shipmentId: ShipmentId, specimen: Specimen): DomainValidation[ShipmentSpecimen]

  def getBySpecimens(shipmentId: ShipmentId, specimens: Specimen*): DomainValidation[List[ShipmentSpecimen]]

  def shipmentSpecimenCount(shipmentId: ShipmentId, state: ShipmentItemState): Int
}

@Singleton
class ShipmentSpecimensWriteRepositoryImpl @Inject()(val testData: TestData)
    extends StmReadWriteRepositoryImpl[ShipmentSpecimenId, ShipmentSpecimen](v => v.id)
    with ShipmentSpecimensWriteRepository {
  import org.biobank.CommonValidations._

  override def init(): Unit = {
    super.init()
    testData.testShipmentSpecimens.foreach(put)
  }

  def nextIdentity: ShipmentSpecimenId = new ShipmentSpecimenId(nextIdentityAsString)

  protected def notFound(id: ShipmentSpecimenId): IdNotFound = IdNotFound(s"shipment specimen id: $id")

  def shipmentAndSpecimenNotFound(shipmentId: ShipmentId, specimenId: SpecimenId): IdNotFound =
    IdNotFound(s"shipment id: $shipmentId, specimen id: $specimenId")

  def specimenNotFound(specimen: Specimen): String =
    IdNotFound(s"shipment specimen with inventory ID: ${specimen.inventoryId}").toString

  def forShipment(id: ShipmentId): Set[ShipmentSpecimen] =
    getValues.filter { ss =>
      ss.shipmentId == id
    }.toSet

  def allForSpecimen(id: SpecimenId): Set[ShipmentSpecimen] =
    getValues.filter { ss =>
      ss.specimenId == id
    }.toSet

  def getBySpecimen(shipmentId: ShipmentId, specimen: Specimen): DomainValidation[ShipmentSpecimen] =
    getValues
      .find(ss => (ss.shipmentId == shipmentId) && (ss.specimenId == specimen.id)).toSuccessNel(
        specimenNotFound(specimen)
      )

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getBySpecimens(shipmentId: ShipmentId, specimens: Specimen*): DomainValidation[List[ShipmentSpecimen]] =
    specimens.map(getBySpecimen(shipmentId, _)).toList.sequenceU

  def shipmentSpecimenCount(shipmentId: ShipmentId, state: ShipmentItemState): Int =
    forShipment(shipmentId).filter(ss => ss.state == state).size

}
