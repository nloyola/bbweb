package org.biobank.domain.containers

import play.api.libs.json._
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import org.biobank.domain.AnatomicalSourceType._
import org.biobank.domain.PreservationType._
import org.biobank.domain.SpecimenType._
import scalaz.Scalaz._

/**
 * Optional information for a [[Container]] that restricts the types of
 * [[domain.AnatomicalSourceType AnatomicalSourceTypes]], [[domain.SpecimenType
 * SpecimenTypes]], and [[domain.PreservationType PreservationTypes]]. An empty set means that any
 * of that type is allowed in the associated [[Container]] and its children, but a non-empty set means only the
 * specified types are allowed.
 */
final case class ContainerConstraints(id:                    ContainerConstraintsId,
                                      slug:                  Slug,
                                      name:                  String,
                                      description:           Option[String],
                                      centreId:              Option[CentreId],
                                      anatomicalSourceTypes: Set[AnatomicalSourceType],
                                      preservationTypes:     Set[PreservationType],
                                      specimenTypes:         Set[SpecimenType])
    extends IdentifiedValueObject[ContainerConstraintsId]
    with HasSlug
    with HasName
    with HasOptionalDescription
{
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  /** Used to change the name. */
  def withName(name: String): DomainValidation[ContainerConstraints] = {
    validateNonEmptyString(name, InvalidName) map { _ =>
      copy(name         = name)
    }
  }

  /** Used to change the description. */
  def withDescription(description: Option[String]): DomainValidation[ContainerConstraints] = {
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      copy(description  = description)
    }
  }

  def withCentre(centreId: Option[CentreId]): DomainValidation[ContainerConstraints] = {
    validateIdOption(centreId, CentreIdRequired) map { _ =>
      copy(centreId = centreId)
    }
  }

  def withAnatomicalSourceTypes(sources: Set[AnatomicalSourceType]): DomainValidation[ContainerConstraints] = {
    copy(anatomicalSourceTypes = sources).success
  }

  def withPreservationTypes(preservations: Set[PreservationType]): DomainValidation[ContainerConstraints] = {
    copy(preservationTypes = preservations).success
  }

  def withSpecimenTypes(specimenTypes: Set[SpecimenType]): DomainValidation[ContainerConstraints] = {
    copy(specimenTypes = specimenTypes).success
  }

}

object ContainerConstraints {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(id:                    ContainerConstraintsId,
             name:                  String,
             description:           Option[String],
             centreId:              Option[CentreId],
             anatomicalSourceTypes: Set[AnatomicalSourceType],
             preservationTypes:     Set[PreservationType],
             specimenTypes:         Set[SpecimenType]): DomainValidation[ContainerConstraints] = {
    validate(id, name, description, centreId) map { _ =>
      ContainerConstraints(
        id                    = id,
        slug                  = Slug(name),
        name                  = name,
        description           = description,
        centreId              = centreId,
        anatomicalSourceTypes = anatomicalSourceTypes,
        preservationTypes     = preservationTypes,
        specimenTypes         = specimenTypes)
    }
  }

  def validate(id:          ContainerConstraintsId,
               name:        String,
               description: Option[String],
               centreId:    Option[CentreId]): DomainValidation[Unit] = {
    (validateId(id) |@|
       validateNonEmptyString(name, InvalidName) |@|
       validateNonEmptyStringOption(description, InvalidDescription) |@|
       validateIdOption(centreId, CentreIdRequired)) {
      case _ => ()
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(constraints: ContainerConstraints): DomainValidation[Unit] = {
    validate(constraints.id, constraints.name, constraints.description, constraints.centreId)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(constraints: Option[ContainerConstraints]): DomainValidation[Unit] = {
    constraints match {
      case Some(c) => validate(c.id, c.name, c.description, c.centreId)
      case None => ().success
    }
  }

  implicit val specimenContainerFormat: Format[ContainerConstraints] = Json.format[ContainerConstraints]

}
