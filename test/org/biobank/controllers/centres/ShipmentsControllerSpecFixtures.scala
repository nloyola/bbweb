package org.biobank.controllers.centres

import org.biobank.domain.centres._
import org.biobank.fixtures.ControllerFixture
import org.biobank.domain.participants._
import org.biobank.dto.centres.ShipmentSpecimenDto

abstract private[centres] class ShipmentsControllerSpecFixtures
    extends ControllerFixture with ShipmentSpecFixtures {

  override def centresFixture = {
    val fixture = super.centresFixture
    centreRepository.put(fixture.originCentre)
    centreRepository.put(fixture.destinationCentre)
    fixture
  }

  override def specimensFixture(numSpecimens: Int) = {
    val f = super.specimensFixture(numSpecimens)

    centreRepository.put(f.originCentre)
    centreRepository.put(f.destinationCentre)
    studyRepository.put(f.study)
    collectionEventTypeRepository.put(f.ceventType)
    participantRepository.put(f.participant)
    collectionEventRepository.put(f.cevent)
    f.specimens.foreach { specimen =>
      specimenRepository.put(specimen)
      ceventSpecimenRepository.put(CeventSpecimen(f.cevent.id, specimen.id))
    }
    shipmentsReadRepository.put(f.shipmentDto)
    shipmentsWriteRepository.put(f.shipment)
    f
  }

  override def shipmentSpecimensFixture(numSpecimens: Int) = {
    val f = super.shipmentSpecimensFixture(numSpecimens)
    f.shipmentSpecimenMap.values.foreach { v =>
      specimenRepository.put(v.specimen)
      shipmentSpecimensRepository.put(v.shipmentSpecimen)
      shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(v.shipmentSpecimen))
    }
    f
  }

  override def addSpecimenToShipment(shipment: Shipment, originCentre: Centre) = {
    val f = super.addSpecimenToShipment(shipment, originCentre)
    specimenRepository.put(f.specimen)
    shipmentSpecimensRepository.put(f.shipmentSpecimen)
    shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(f.shipmentSpecimen))
    f
  }

}
