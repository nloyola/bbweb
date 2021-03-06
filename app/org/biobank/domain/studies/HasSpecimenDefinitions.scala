package org.biobank.domain.studies

import org.biobank.domain._
import scalaz.Scalaz._

trait HasSpecimenDefinitions[T <: SpecimenDefinition] {

  import org.biobank.CommonValidations._

  val specimenDefinitions: Set[T]

  def specimenDefinition(id: SpecimenDefinitionId): DomainValidation[T] =
    specimenDefinitions.find(_.id == id).toSuccessNel(IdNotFound(s"specimen definition: $id").toString)

  protected def checkAddSpecimenDefinition(specimenDefinition: T): DomainValidation[Unit] =
    nameNotUsed(specimenDefinition).map { _ =>
      ()
    }

  protected def checkRemoveSpecimenDefinition(
      specimenDefinitionId: SpecimenDefinitionId
    ): DomainValidation[T] =
    specimenDefinitions
      .find { x =>
        x.id == specimenDefinitionId
      }
      .toSuccessNel(IdNotFound(s"specimen definition ID: $specimenDefinitionId").toString)

  protected def nameNotUsed(specimenDefinition: SpecimenDefinition): DomainValidation[Unit] = {
    val nameLowerCase = specimenDefinition.name.toLowerCase
    specimenDefinitions
      .find { x =>
        (x.name.toLowerCase == nameLowerCase) && (x.id != specimenDefinition.id)
      } match {
      case Some(_) =>
        EntityCriteriaError(s"specimen definition name already used: ${specimenDefinition.name}")
          .failureNel[Unit]
      case None =>
        ().successNel[DomainError]
    }
  }

}
