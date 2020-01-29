package org.biobank.dto.containers

import org.biobank.domain.{HasName, HasOptionalDescription}
import org.biobank.domain.AnatomicalSourceType._
import org.biobank.domain.PreservationType._
import org.biobank.domain.SpecimenType._
import org.biobank.domain.containers.ContainerConstraints
import play.api.libs.json._

final case class ContainerConstraintsDto(
    name:              String,
    description:       Option[String],
    anatomicalSources: Set[AnatomicalSourceType],
    preservationTypes: Set[PreservationType],
    specimenTypes:     Set[SpecimenType])
    extends HasName with HasOptionalDescription

object ContainerConstraintsDto {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def apply(constraints: ContainerConstraints): ContainerConstraintsDto =
    ContainerConstraintsDto(name              = constraints.name,
                            description       = constraints.description,
                            anatomicalSources = constraints.anatomicalSources,
                            preservationTypes = constraints.preservationTypes,
                            specimenTypes     = constraints.specimenTypes)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerConstraintsDtoFormat: Format[ContainerConstraintsDto] =
    Json.format[ContainerConstraintsDto]

}
