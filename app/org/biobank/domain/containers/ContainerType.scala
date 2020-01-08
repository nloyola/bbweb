package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.ValidationKey
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import play.api.libs.json._
import scalaz.Scalaz._

/**
 * Predicates that can be used to filter collections of container types.
 *
 */
trait ContainerTypePredicates {

  type ContainerTypeFilter = ContainerType => Boolean

}

trait ContainerTypeValidations {
  val NameMinLength: Long = 2L

  case object ContainerSchemaIdInvalid extends ValidationKey

}

/**
 * Describes a container configuration which may hold child [[Container Containers]] or
 * [[org.biobank.domain.participants.Specimen Specimens]]. Container types are used to create a representation
 * of a physical container.
 */
sealed trait ContainerType
    extends ConcurrencySafeEntity[ContainerTypeId] with HasUniqueName with HasSlug with HasOptionalDescription
    with ContainerValidations {

  /**
   * The [[domain.centres.Centre Centre]] that owns and is allowed to modify this [[ContainerType]].
   */
  val centreId: CentreId

  /**
   * How [[Container Containers]] of this [[ContainerType]] are designed and laid out, with labelled positions
   * for children.
   */
  val schemaId: ContainerSchemaId

  /**
   * True if this [[ContainerType]] can be used by (but not modified) by other
   * [[domain.centres.Centre Centres]], otherwise false.
   */
  val shared: Boolean

  /**
   * True if this [[ContainerType]] can be used to create new [[Container Containers]], or false if this
   * [[ContainerType]] is to be used only for existing [[Container Containers]].
   */
  val enabled: Boolean

  val storageType: ContainerStorageType

  def withName(name: String): DomainValidation[ContainerType]

  def withDescription(description: Option[String]): DomainValidation[ContainerType]

  def withCentre(centreId: CentreId): DomainValidation[ContainerType]

  def withSchema(schemaId: ContainerSchemaId): DomainValidation[ContainerType]

  def withShared(shared: Boolean): DomainValidation[ContainerType]

  def withEnabled(enabled: Boolean): DomainValidation[ContainerType]

  override def toString: String =
    s"""|ContainerType:{
        |  id:          $id,
        |  slug:        $slug,
        |  name:        $name,
        |  description: $description,
        |  centreId:    $centreId,
        |  schemaId:    $schemaId,
        |  shared:      $shared,
        |  enabled:     $enabled
        |}""".stripMargin
}

object ContainerType {

  type ContainerTypesCompare = (ContainerType, ContainerType) => Boolean

  val containerStorageType: ContainerStorageType = new ContainerStorageType("storage-container-type")
  val specimenStorageType:  ContainerStorageType = new ContainerStorageType("specimen-container-type")

  val sort2Compare: Map[String, ContainerTypesCompare] =
    Map[String, ContainerTypesCompare]("name" -> ContainerType.compareByName)

  def compareByName(a: ContainerType, b: ContainerType): Boolean =
    (a.name compareToIgnoreCase b.name) < 0

  implicit val containerTypeFormat: Format[ContainerType] = new Format[ContainerType] {

    override def writes(containerType: ContainerType): JsValue =
      Json.obj("id"           -> containerType.id,
               "centreId"     -> containerType.centreId,
               "schemaId"     -> containerType.schemaId,
               "version"      -> containerType.version,
               "timeAdded"    -> containerType.timeAdded,
               "timeModified" -> containerType.timeModified,
               "name"         -> containerType.name,
               "description"  -> containerType.description,
               "shared"       -> containerType.shared,
               "storageType"  -> containerType.getClass.getSimpleName)

    override def reads(json: JsValue): JsResult[ContainerType] = (json \ "status") match {
      case JsDefined(JsString(containerStorageType.id)) => json.validate[StorageContainerType]
      case JsDefined(JsString(specimenStorageType.id))  => json.validate[SpecimenContainerType]
      case _                                            => JsError("error")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val storageContainerTypeFormat: Format[StorageContainerType] = Json.format[StorageContainerType]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val specimenContainerTypeFormat: Format[SpecimenContainerType] = Json.format[SpecimenContainerType]

}

/**
 * Represents a type or classification of storage containers that holds other
 * storage containers, such as, freezers, dewars, hotels, pallets, etc.
 */
final case class StorageContainerType(
    id:           ContainerTypeId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    centreId:     CentreId,
    schemaId:     ContainerSchemaId,
    shared:       Boolean,
    enabled:      Boolean)
    extends { val storageType = ContainerType.containerStorageType } with ContainerType {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def withName(name: String): DomainValidation[StorageContainerType] =
    validateNonEmptyString(name, InvalidName) map { _ =>
      update.copy(name = name, slug = Slug(name))
    }

  def withDescription(description: Option[String]): DomainValidation[StorageContainerType] =
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      update.copy(description = description)
    }

  def withCentre(centreId: CentreId): DomainValidation[StorageContainerType] =
    validateId(centreId, CentreIdRequired) map { _ =>
      update.copy(centreId = centreId)
    }

  def withSchema(schemaId: ContainerSchemaId): DomainValidation[StorageContainerType] =
    validateId(schemaId, ContainerSchemaIdInvalid) map { _ =>
      update.copy(schemaId = schemaId)
    }

  def withShared(shared: Boolean): DomainValidation[StorageContainerType] =
    update.copy(shared = shared).success

  def withEnabled(enabled: Boolean): DomainValidation[StorageContainerType] =
    update.copy(enabled = enabled).success

  private def update(): StorageContainerType =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

}

object StorageContainerType extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(
      id:          ContainerTypeId,
      version:     Long,
      name:        String,
      description: Option[String],
      centreId:    CentreId,
      schemaId:    ContainerSchemaId,
      shared:      Boolean,
      enabled:     Boolean
    ): DomainValidation[StorageContainerType] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateNonEmptyString(name, InvalidName) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateId(centreId, CentreIdRequired) |@|
      validateId(schemaId, ContainerSchemaIdInvalid)) {
      case _ =>
        StorageContainerType(id           = id,
                             version      = version,
                             timeAdded    = OffsetDateTime.now,
                             timeModified = None,
                             slug         = Slug(name),
                             name         = name,
                             description  = description,
                             centreId     = centreId,
                             schemaId     = schemaId,
                             shared       = shared,
                             enabled      = enabled)
    }

}

/**
 * Represents a type or classification of specimen containers that <em>directly</em> hold [[Specimen
 * Specimens]], such as, specific types of tubes (e.g. NUNC 2ml, NUNC 5ml, etc.), vials, slides, well plates,
 * etc..
 */
final case class SpecimenContainerType(
    id:           ContainerTypeId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    centreId:     CentreId,
    schemaId:     ContainerSchemaId,
    shared:       Boolean,
    enabled:      Boolean)
    extends { val storageType = ContainerType.specimenStorageType } with ContainerType {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def withName(name: String): DomainValidation[SpecimenContainerType] =
    validateNonEmptyString(name, InvalidName) map { _ =>
      update.copy(name = name, slug = Slug(name))
    }

  def withDescription(description: Option[String]): DomainValidation[SpecimenContainerType] =
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      update.copy(description = description)
    }

  def withCentre(centreId: CentreId): DomainValidation[SpecimenContainerType] =
    validateId(centreId, CentreIdRequired) map { _ =>
      update.copy(centreId = centreId)
    }

  def withSchema(schemaId: ContainerSchemaId): DomainValidation[SpecimenContainerType] =
    validateId(schemaId, ContainerSchemaIdInvalid) map { _ =>
      update.copy(schemaId = schemaId)
    }

  def withShared(shared: Boolean): DomainValidation[SpecimenContainerType] =
    update.copy(shared = shared).success

  def withEnabled(enabled: Boolean): DomainValidation[SpecimenContainerType] =
    update.copy(enabled = enabled).success

  private def update() =
    copy(version = nextVersion, timeModified = Some(OffsetDateTime.now))

}

object SpecimenContainerType extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(
      id:          ContainerTypeId,
      version:     Long,
      name:        String,
      description: Option[String],
      centreId:    CentreId,
      schemaId:    ContainerSchemaId,
      shared:      Boolean,
      enabled:     Boolean
    ): DomainValidation[SpecimenContainerType] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateNonEmptyString(name, InvalidName) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateId(centreId, CentreIdRequired) |@|
      validateId(schemaId, ContainerSchemaIdInvalid)) {
      case _ =>
        SpecimenContainerType(id           = id,
                              centreId     = centreId,
                              schemaId     = schemaId,
                              version      = 0L,
                              timeAdded    = OffsetDateTime.now,
                              timeModified = None,
                              slug         = Slug(name),
                              name         = name,
                              description  = description,
                              shared       = shared,
                              enabled      = enabled)
    }
}
