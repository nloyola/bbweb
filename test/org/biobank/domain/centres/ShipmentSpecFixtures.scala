package org.biobank.domain.centres

import java.time.OffsetDateTime
import org.biobank.domain.{EntityState, Factory, Location}
import org.biobank.domain.studies._
import org.biobank.domain.participants._
import org.scalatest.Assertions._

trait ShipmentSpecFixtures {

  class ToFromCentres(val originCentre: Centre, val destinationCentre: Centre)

  class ShipmentFixture[T <: Shipment](originCentre: Centre, destinationCentre: Centre, val shipment: T)
      extends ToFromCentres(originCentre, destinationCentre)

  class ShipmentsFixture(
      originCentre:      Centre,
      destinationCentre: Centre,
      val shipmentMap:   Map[ShipmentId, Shipment])
      extends ToFromCentres(originCentre, destinationCentre)

  class ShipmentsByStateFixture[T <: Shipment](
      originCentre:      Centre,
      destinationCentre: Centre,
      val shipments:     Map[EntityState, Shipment])
      extends ToFromCentres(originCentre, destinationCentre)

  class CreatedShipmentsFixture(
      originCentre:      Centre,
      destinationCentre: Centre,
      val shipmentMap:   Map[ShipmentId, CreatedShipment])
      extends ToFromCentres(originCentre, destinationCentre)

  class SpecimenShipmentSpecimen(val specimen: UsableSpecimen, val shipmentSpecimen: ShipmentSpecimen)

  class CollectionEventFixture {
    val study              = factory.createEnabledStudy
    val specimenDefinition = factory.createCollectedSpecimenDefinition

    val ceventType = factory.createCollectionEventType
      .copy(studyId = study.id, specimenDefinitions = Set(specimenDefinition), annotationTypes = Set.empty)
    val participant = factory.createParticipant.copy(studyId = study.id)
    val cevent      = factory.createCollectionEvent
  }

  class SpecimensFixture(
      originCentre:            Centre,
      destinationCentre:       Centre,
      shipment:                Shipment,
      val originLocation:      Location,
      val destinationLocation: Location,
      val study:               Study,
      val specimenDefinition:  CollectedSpecimenDefinition,
      val ceventType:          CollectionEventType,
      val participant:         Participant,
      val cevent:              CollectionEvent,
      val specimens:           List[UsableSpecimen])
      extends ShipmentFixture(originCentre, destinationCentre, shipment)

  case class ShipmentSpecimenData(val specimen: UsableSpecimen, val shipmentSpecimen: ShipmentSpecimen)

  class ShipmentSpecimensFixture(
      originCentre:            Centre,
      destinationCentre:       Centre,
      shipment:                Shipment,
      study:                   Study,
      specimenDefinition:      CollectedSpecimenDefinition,
      ceventType:              CollectionEventType,
      participant:             Participant,
      cevent:                  CollectionEvent,
      specimens:               List[UsableSpecimen],
      val shipmentSpecimenMap: Map[SpecimenId, ShipmentSpecimenData])
      extends SpecimensFixture(originCentre,
                               destinationCentre,
                               shipment,
                               originCentre.locations.head,
                               destinationCentre.locations.head,
                               study,
                               specimenDefinition,
                               ceventType,
                               participant,
                               cevent,
                               specimens)

  protected val factory: Factory

  val nonCreatedStates = List(Shipment.packedState,
                              Shipment.sentState,
                              Shipment.receivedState,
                              Shipment.unpackedState,
                              Shipment.lostState)

  def centresFixture = {
    val centres = (1 to 2).map { _ =>
      val location = factory.createLocation
      factory.createEnabledCentre.copy(locations = Set(location))
    }
    new ToFromCentres(centres(0), centres(1))
  }

  def createdShipmentFixture = {
    val f = centresFixture
    new ShipmentFixture(f.originCentre,
                        f.destinationCentre,
                        factory.createShipment(f.originCentre, f.destinationCentre))
  }

  def makePackedShipment(shipment: Shipment): PackedShipment =
    shipment match {
      case s: CreatedShipment => s.pack(OffsetDateTime.now)
      case _ => fail(s"bad shipment state: ${shipment.state}")
    }

  def makeSentShipment(shipment: Shipment): SentShipment =
    makePackedShipment(shipment)
      .send(OffsetDateTime.now).fold(err => fail("could not make a sent shipment"), s => s)

  def makeReceivedShipment(shipment: Shipment): ReceivedShipment =
    makeSentShipment(shipment)
      .receive(OffsetDateTime.now).fold(err => fail("could not make a received shipment"), s => s)

  def makeUnpackedShipment(shipment: Shipment): UnpackedShipment =
    makeReceivedShipment(shipment)
      .unpack(OffsetDateTime.now).fold(err => fail("could not make a unpacked shipment"), s => s)

  def makeLostShipment(shipment: Shipment): LostShipment =
    makeSentShipment(shipment).lost

  def createdShipmentsFixture(numShipments: Int) = {
    val f = centresFixture
    new CreatedShipmentsFixture(f.originCentre, f.destinationCentre, (1 to numShipments).map { _ =>
      val shipment = factory.createShipment(f.originCentre, f.destinationCentre)
      shipment.id -> shipment
    }.toMap)
  }

  def packedShipmentFixture = {
    val f = createdShipmentFixture
    new ShipmentFixture(originCentre      = f.originCentre,
                        destinationCentre = f.destinationCentre,
                        shipment          = makePackedShipment(f.shipment))
  }

  def sentShipmentFixture = {
    val f = createdShipmentFixture

    new ShipmentFixture(originCentre      = f.originCentre,
                        destinationCentre = f.destinationCentre,
                        shipment          = makeSentShipment(f.shipment))
  }

  def receivedShipmentFixture = {
    val f = createdShipmentFixture
    new ShipmentFixture(originCentre      = f.originCentre,
                        destinationCentre = f.destinationCentre,
                        shipment          = makeReceivedShipment(f.shipment))
  }

  def unpackedShipmentFixture = {
    val f = createdShipmentFixture
    new ShipmentFixture(originCentre      = f.originCentre,
                        destinationCentre = f.destinationCentre,
                        shipment          = makeUnpackedShipment(f.shipment))
  }

  def lostShipmentFixture = {
    val f = createdShipmentFixture
    new ShipmentFixture(originCentre      = f.originCentre,
                        destinationCentre = f.destinationCentre,
                        shipment          = makeLostShipment(f.shipment))
  }

  def allShipmentsFixture = {
    val centres           = centresFixture
    val originCentre      = centres.originCentre
    val destinationCentre = centres.destinationCentre
    new ShipmentsByStateFixture(originCentre      = centres.originCentre,
                                destinationCentre = centres.destinationCentre,
                                shipments = Map(
                                  Shipment.createdState -> factory.createShipment(originCentre,
                                                                                  destinationCentre),
                                  Shipment.packedState -> factory.createPackedShipment(originCentre,
                                                                                       destinationCentre),
                                  Shipment.sentState -> factory.createSentShipment(originCentre,
                                                                                   destinationCentre),
                                  Shipment.receivedState -> factory.createReceivedShipment(originCentre,
                                                                                           destinationCentre),
                                  Shipment.unpackedState -> factory.createUnpackedShipment(originCentre,
                                                                                           destinationCentre),
                                  Shipment.completedState -> factory
                                    .createCompletedShipment(originCentre, destinationCentre),
                                  Shipment.lostState -> factory.createLostShipment(originCentre,
                                                                                   destinationCentre)
                                ))
  }

  def specimensFixture(numSpecimens: Int) = {
    val f                   = createdShipmentFixture
    val ceventFixture       = new CollectionEventFixture
    val originLocation      = f.originCentre.locations.head
    val destinationLocation = f.destinationCentre.locations.head

    val specimens = (1 to numSpecimens).map { _ =>
      factory.createUsableSpecimen.copy(originLocationId = originLocation.id, locationId = originLocation.id)
    }.toList

    new SpecimensFixture(originCentre        = f.originCentre,
                         originLocation      = originLocation,
                         destinationCentre   = f.destinationCentre,
                         destinationLocation = destinationLocation,
                         study               = ceventFixture.study,
                         specimenDefinition  = ceventFixture.specimenDefinition,
                         ceventType          = ceventFixture.ceventType,
                         participant         = ceventFixture.participant,
                         cevent              = ceventFixture.cevent,
                         specimens           = specimens,
                         shipment            = f.shipment)
  }

  def shipmentSpecimensFixture(numSpecimens: Int) = {
    val f = specimensFixture(numSpecimens)

    val map = f.specimens.zipWithIndex.map {
      case (specimen, index) =>
        val updatedSpecimen = specimen.copy(inventoryId = s"inventoryId_$index")
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id, specimenId = specimen.id)
        (updatedSpecimen.id, new ShipmentSpecimenData(updatedSpecimen, shipmentSpecimen))
    }.toMap

    new ShipmentSpecimensFixture(originCentre        = f.originCentre,
                                 destinationCentre   = f.destinationCentre,
                                 study               = f.study,
                                 specimenDefinition  = f.specimenDefinition,
                                 ceventType          = f.ceventType,
                                 participant         = f.participant,
                                 cevent              = f.cevent,
                                 specimens           = f.specimens,
                                 shipment            = f.shipment,
                                 shipmentSpecimenMap = map)
  }

  def addSpecimenToShipment(shipment: Shipment, originCentre: Centre) = {
    val specimen = factory.createUsableSpecimen
      .copy(originLocationId = originCentre.locations.head.id, locationId = originCentre.locations.head.id)
    val shipmentSpecimen =
      factory.createShipmentSpecimen.copy(shipmentId = shipment.id, specimenId = specimen.id)
    new SpecimenShipmentSpecimen(specimen, shipmentSpecimen)
  }

}
