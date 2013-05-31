package domain.study

import scalaz._
import scalaz.Scalaz._
import domain.AnatomicalSourceId
import domain.Entity
import domain.Identity
import domain.PreservationId
import domain.DomainValidation
import domain.DomainError
import domain.UserId

sealed abstract class Study extends Entity[StudyId] {
  def name: String
  def description: String
  def specimenGroups: Map[SpecimenGroupId, SpecimenGroup]

  override def toString =
    "{ id:%s, name:%s, description:%s }" format (id.toString, name, description)
}

object Study {
  val invalidVersionMessage = "study %s: expected version %s doesn't match current version %s"

  def invalidVersion(studyId: Identity, expected: Long, current: Long) =
    DomainError(invalidVersionMessage format (studyId, expected, current))

  def requireVersion[T <: Study](study: T, expectedVersion: Option[Long]): DomainValidation[T] = {
    val id = study.id
    val version = study.version

    expectedVersion match {
      case Some(expected) if (version != expected) => invalidVersion(id, expected, version).fail
      case Some(expected) if (version == expected) => study.success
      case None => study.success
    }
  }

  // TODO: not sure yet if this is the right place for this method
  def nextIdentity: StudyId =
    new StudyId(java.util.UUID.randomUUID.toString.toUpperCase)

  def add(id: StudyId, name: String, description: String): DomainValidation[DisabledStudy] =
    DisabledStudy(id, version = 0L, name, description, specimenGroups = Map.empty).success

}

case class DisabledStudy(id: StudyId, version: Long = -1, name: String, description: String,
  specimenGroups: Map[SpecimenGroupId, SpecimenGroup] = Map.empty)
  extends Study {

  def addSpecimenGroup(specimenGroup: SpecimenGroup): DomainValidation[DisabledStudy] = {
    copy(version = version + 1,
      specimenGroups = specimenGroups + (specimenGroup.id -> specimenGroup)).success
  }

  def updateSpecimenGroup(specimenGroup: SpecimenGroup): DomainValidation[DisabledStudy] =
    specimenGroups.get(specimenGroup.id) match {
      case Some(sg) =>
        copy(version = version + 1,
          specimenGroups = specimenGroups + (specimenGroup.id -> specimenGroup)).success
      case None => DomainError("specimen group with ID not found: %s" format specimenGroup.id).fail
    }

  def addCollectionEventType(name: String, description: String, recurring: Boolean) = {

  }

}

case class EnabledStudy(id: StudyId, version: Long = -1, name: String, description: String,
  specimenGroups: Map[SpecimenGroupId, SpecimenGroup] = Map.empty)
  extends Study {

}

