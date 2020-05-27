package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.domain._
import play.api.libs.json._
import org.biobank.domain.centres.CentreId
import scalaz.Scalaz._
import scalaz.NonEmptyList._

/**
 * Predicates that can be used to filter collections of [[ContainerSchema ContainerSchemas]].
 *
 */
trait ContainerSchemaPredicates extends HasNamePredicates[ContainerSchema] {

  type ContainerSchemaFilter = ContainerSchema => Boolean

}

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
    labels:       Set[String])
    extends ConcurrencySafeEntity[ContainerSchemaId] with HasSlug with HasUniqueName
    with HasOptionalDescription with ContainerValidations {
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

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def withLabels(labels: Set[String]): DomainValidation[ContainerSchema] =
    labels
      .map { validateNonEmptyString(_, ContainerSchemaLabelInvalid) }
      .toList.sequenceU
      .map { l =>
        update.copy(labels = labels)
      }

  def isLabelValid(label: String): Boolean = labels.exists(_ == label)

  def getLabel(label: String): DomainValidation[ContainerSchemaLabel] =
    labels
      .find(_ == label).map(l => ContainerSchemaLabel(schemaId = id, label = l))
      .toSuccessNel(EntityCriteriaError(s"label is invalid on schema with name $name").toString)

  private def update(): ContainerSchema =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

  override def toString: String = Json.prettyPrint(Json.toJson(this))
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
                        labels       = Set.empty[String])
    }

  type ContainerSchemasCompare = (ContainerSchema, ContainerSchema) => Boolean

  val sort2Compare: Map[String, ContainerSchemasCompare] =
    Map[String, ContainerSchemasCompare]("name" -> ContainerSchema.compareByName)

  def compareByName(a: ContainerSchema, b: ContainerSchema): Boolean =
    (a.name compareToIgnoreCase b.name) < 0

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val containerSchemaFormat: Format[ContainerSchema] = Json.format[ContainerSchema]
}
