package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.domain._
import play.api.libs.json._
import scalaz.Scalaz._
import org.biobank.domain.centres.CentreId

/**
 * A plan for how the children in a {@link Container} are positioned and labelled.
 */
final case class ContainerSchema(
    id:           ContainerSchemaId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    shared:       Boolean,
    centreId:     CentreId,
    positions:    Set[ContainerSchemaPosition])
    extends ConcurrencySafeEntity[ContainerSchemaId] with HasSlug with HasUniqueName
    with HasOptionalDescription {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  /** Used to change the name. */
  def withName(name: String): DomainValidation[ContainerSchema] =
    validateNonEmptyString(name, InvalidName) map { _ =>
      update.copy(name = name)
    }

  /** Used to change the description. */
  def withDescription(description: Option[String]): DomainValidation[ContainerSchema] =
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      update.copy(description = description)
    }

  def withShared(shared: Boolean): DomainValidation[ContainerSchema] =
    update.copy(shared = shared).successNel[String]

  def withCentre(centreId: CentreId): DomainValidation[ContainerSchema] =
    validateId(centreId, InvalidCentreId).map { _ =>
      update.copy(centreId = centreId)
    }

  def withPositions(positions: Set[ContainerSchemaPosition]): DomainValidation[ContainerSchema] = {
    val positionsWithSchemaId = positions.map { _.copy(schemaId = id) }
    update.copy(positions = positionsWithSchemaId).successNel[String]
  }

  //@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def getPosition(label: String): DomainValidation[ContainerSchemaPosition] =
    positions
      .find(_.label == label)
      .toSuccessNel(s"invalid position label: $label --> $id")

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def getPosition(positionId: ContainerSchemaPositionId): DomainValidation[ContainerSchemaPosition] =
    positions
      .find(_.id == positionId)
      .toSuccessNel(s"invalid position id: $positionId")

  private def update(): ContainerSchema =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

  override def toString: String =
    s"""|${this.getClass.getSimpleName}: {
        |  id:              $id,
        |  version:         $version,
        |  timeAdded:       $timeAdded,
        |  timeModified:    $timeModified,
        |  slug:            $slug,
        |  name:            $name,
        |  description:     $description,
        |  centreId:        $centreId,
        |  positions:       $positions""".stripMargin
}

/**
 * Factory object used to create a container schema.
 */
object ContainerSchema {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  /**
   * The factory method to create a container schema.
   *
   * Performs validation on fields.
   */
  def create(
      id:          ContainerSchemaId,
      version:     Long,
      name:        String,
      description: Option[String],
      shared:      Boolean,
      centreId:    CentreId
    ): DomainValidation[ContainerSchema] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateNonEmptyString(name, InvalidName) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateId(centreId, InvalidCentreId)) {
      case _ =>
        ContainerSchema(id           = id,
                        version      = version,
                        timeAdded    = OffsetDateTime.now,
                        timeModified = None,
                        slug         = Slug(name),
                        name         = name,
                        description  = description,
                        shared       = shared,
                        centreId     = centreId,
                        positions    = Set.empty[ContainerSchemaPosition])
    }

  implicit val containerSchemaFormat: Format[ContainerSchema] = Json.format[ContainerSchema]
}
