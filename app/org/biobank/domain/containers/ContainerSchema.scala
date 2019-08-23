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
    centreId:     CentreId)
    extends ConcurrencySafeEntity[ContainerSchemaId] with HasUniqueName with HasOptionalDescription {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  /** Used to change the name. */
  def withName(name: String): DomainValidation[ContainerSchema] =
    validateNonEmptyString(name, InvalidName) map { _ =>
      copy(name = name, version = version + 1, timeModified = Some(OffsetDateTime.now))
    }

  /** Used to change the description. */
  def withDescription(description: Option[String]): DomainValidation[ContainerSchema] =
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      copy(description = description, version = version + 1, timeModified = Some(OffsetDateTime.now))
    }

  def withShared(shared: Boolean): DomainValidation[ContainerSchema] =
    copy(shared = shared, version = version + 1, timeModified = Some(OffsetDateTime.now)).successNel[String]

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
                        centreId     = centreId)
    }

  implicit val containerSchemaFormat: Format[ContainerSchema] = Json.format[ContainerSchema]
}
