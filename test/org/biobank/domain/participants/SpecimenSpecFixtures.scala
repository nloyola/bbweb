package org.biobank.domain.participants

import org.biobank.domain.Factory
import org.scalatest.OptionValues._
import org.biobank.dto.centres.CentreLocationInfo
import scala.language.reflectiveCalls

trait SpecimenSpecFixtures {

  protected val factory: Factory

  protected def createEntities() = {
    val _centre             = factory.createEnabledCentre
    val _study              = factory.createEnabledStudy
    val _specimenDefinition = factory.createCollectedSpecimenDefinition
    val _ceventType = factory.createCollectionEventType
      .copy(studyId = _study.id, specimenDefinitions = Set(_specimenDefinition), annotationTypes = Set.empty)
    val _participant        = factory.createParticipant.copy(studyId = _study.id)
    val _cevent             = factory.createCollectionEvent
    val _centreLocationInfo = CentreLocationInfo(_centre, _centre.locations.headOption.value)

    new {
      val centre             = _centre
      val centreLocationInfo = _centreLocationInfo
      val study              = _study
      val specimenDefinition = _specimenDefinition
      val ceventType         = _ceventType
      val participant        = _participant
      val cevent             = _cevent
    }
  }

  protected def createEntitiesAndSpecimens() = {
    val entities = createEntities

    val _specimens = (1 to 2).map { _ =>
      factory.createUsableSpecimen
    }.toList

    new {
      val centre             = entities.centre
      val centreLocationInfo = entities.centreLocationInfo
      val study              = entities.study
      val participant        = entities.participant
      val ceventType         = entities.ceventType
      val cevent             = entities.cevent
      val specimens          = _specimens
    }
  }
}
