package org.biobank.domain.containers

import play.api.libs.json._
import org.biobank.domain._
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
final case class ContainerConstraints(
    name:              String,
    description:       Option[String],
    anatomicalSources: Set[AnatomicalSourceType],
    preservationTypes: Set[PreservationType],
    specimenTypes:     Set[SpecimenType])
    extends HasName with HasOptionalDescription

object ContainerConstraints {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(
      name:              String,
      description:       Option[String],
      anatomicalSources: Set[AnatomicalSourceType],
      preservationTypes: Set[PreservationType],
      specimenTypes:     Set[SpecimenType]
    ): DomainValidation[ContainerConstraints] =
    validate(name, description) map { _ =>
      ContainerConstraints(name              = name,
                           description       = description,
                           anatomicalSources = anatomicalSources,
                           preservationTypes = preservationTypes,
                           specimenTypes     = specimenTypes)
    }

  def validate(name: String, description: Option[String]): DomainValidation[Unit] =
    (validateNonEmptyString(name, InvalidName) |@|
      validateNonEmptyStringOption(description, InvalidDescription)) {
      case _ => ()
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(constraints: ContainerConstraints): DomainValidation[Unit] =
    validate(constraints.name, constraints.description)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(constraints: Option[ContainerConstraints]): DomainValidation[Unit] =
    constraints match {
      case Some(c) => validate(c.name, c.description)
      case None    => ().success
    }

  implicit val constraintsFormat: Format[ContainerConstraints] = Json.format[ContainerConstraints]

}
