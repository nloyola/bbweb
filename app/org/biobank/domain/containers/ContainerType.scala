package org.biobank.domain.containers

import java.time.OffsetDateTime
import org.biobank.ValidationKey
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import play.api.libs.json._
import scalaz.Scalaz._

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
   * The [[domain.centres.Centre Centre]] that owns and is allowed to modify this
   * [[ContainerType]].
   *
   * When equal to [[https://www.scala-lang.org/api/current/scala/None$.html None]] then it is a globally
   * accessible [[ContainerType]].
   */
  val centreId: Option[CentreId]

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

  def withName(name: String): DomainValidation[ContainerType]

  def withDescription(description: Option[String]): DomainValidation[ContainerType]

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
        |  shared:      $shared
        |}""".stripMargin
}

object ContainerType {

  implicit val containerTypeWrites: Writes[ContainerType] = new Writes[ContainerType] {

    def writes(containerType: ContainerType): JsValue =
      Json.obj("id"           -> containerType.id,
               "centreId"     -> containerType.centreId,
               "schemaId"     -> containerType.schemaId,
               "version"      -> containerType.version,
               "timeAdded"    -> containerType.timeAdded,
               "timeModified" -> containerType.timeModified,
               "name"         -> containerType.name,
               "description"  -> containerType.description,
               "shared"       -> containerType.shared,
               "status"       -> containerType.getClass.getSimpleName)
  }

}

/**
 * When a container type is enabled, it `can` be used to create new containers.
 */
final case class StorageContainerType(
    id:           ContainerTypeId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    centreId:     Option[CentreId],
    schemaId:     ContainerSchemaId,
    shared:       Boolean,
    enabled:      Boolean)
    extends ContainerType {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def withName(name: String): DomainValidation[StorageContainerType] =
    validateNonEmptyString(name, InvalidName) map { _ =>
      update.copy(name = name)
    }

  def withDescription(description: Option[String]): DomainValidation[StorageContainerType] =
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      update.copy(description = description)
    }

  def withShared(shared: Boolean): DomainValidation[StorageContainerType] =
    update.copy(shared = shared).success

  def withEnabled(enabled: Boolean): DomainValidation[StorageContainerType] =
    update.copy(enabled = enabled).success

  private def update(): StorageContainerType =
    copy(version = version + 1L, timeModified = Some(OffsetDateTime.now))

}

object StorageContainerType extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(
      id:          ContainerTypeId,
      version:     Long,
      name:        String,
      description: Option[String],
      centreId:    Option[CentreId],
      schemaId:    ContainerSchemaId,
      shared:      Boolean,
      enabled:     Boolean
    ): DomainValidation[StorageContainerType] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateNonEmptyString(name, InvalidName) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateIdOption(centreId, CentreIdRequired) |@|
      validateId(schemaId, ContainerSchemaIdInvalid)) {
      case _ =>
        StorageContainerType(id           = id,
                             centreId     = centreId,
                             schemaId     = schemaId,
                             version      = version,
                             timeAdded    = OffsetDateTime.now,
                             timeModified = None,
                             slug         = Slug(name),
                             name         = name,
                             description  = description,
                             shared       = shared,
                             enabled      = enabled)
    }

}

/**
 * When a container type is disabled, it ''can not'' be used to create new containers.
 */
final case class SpecimenContainerType(
    id:           ContainerTypeId,
    centreId:     Option[CentreId],
    schemaId:     ContainerSchemaId,
    version:      Long,
    timeAdded:    OffsetDateTime,
    timeModified: Option[OffsetDateTime],
    slug:         Slug,
    name:         String,
    description:  Option[String],
    shared:       Boolean,
    enabled:      Boolean)
    extends ContainerType {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def withName(name: String): DomainValidation[SpecimenContainerType] =
    validateNonEmptyString(name, InvalidName) map { _ =>
      update.copy(name = name)
    }

  def withDescription(description: Option[String]): DomainValidation[SpecimenContainerType] =
    validateNonEmptyStringOption(description, InvalidDescription) map { _ =>
      update.copy(description = description)
    }

  def withShared(shared: Boolean): DomainValidation[SpecimenContainerType] =
    update.copy(shared = shared).success

  def withEnabled(enabled: Boolean): DomainValidation[SpecimenContainerType] =
    update.copy(enabled = enabled).success

  private def update() =
    copy(version = version + 1L, timeModified = Some(OffsetDateTime.now))

}

object SpecimenContainerType extends ContainerValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(
      id:          ContainerTypeId,
      version:     Long,
      name:        String,
      description: Option[String],
      centreId:    Option[CentreId],
      schemaId:    ContainerSchemaId,
      shared:      Boolean,
      enabled:     Boolean
    ): DomainValidation[SpecimenContainerType] =
    (validateId(id) |@|
      validateVersion(version) |@|
      validateIdOption(centreId, CentreIdRequired) |@|
      validateId(schemaId, ContainerSchemaIdInvalid) |@|
      validateNonEmptyString(name, InvalidName) |@|
      validateNonEmptyStringOption(description, InvalidDescription)) {
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
