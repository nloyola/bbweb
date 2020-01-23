package org.biobank.domain.participants

import java.time.OffsetDateTime
import org.biobank.ValidationKey
import org.biobank.domain._
import org.biobank.domain.containers._
import org.biobank.domain.studies._
import play.api.libs.json._
import scalaz.Scalaz._

/**
 * Represents something that was obtained from a [[domain.participants.Participant Participant]] in a
 * [[domain.studies.Study Study]].
 *
 * A Specimen collected from a [[domain.participants.Participant Participant]] can be created with
 * this aggregate and then added to a [[domain.participants.CollectionEvent CollectionEvent]]. When a
 * specimen is created it must be assigned the corresponding [[domain.studies.SpecimenDefinition
 * SpecimenDefinition]] defined in either the [[domain.participants.CollectionEvent
 * CollectionEvent]] or the specimen link type to which it corresponds .
 */
sealed trait Specimen extends ConcurrencySafeEntity[SpecimenId] with HasSlug {

  def state: EntityState

  /** The inventory ID assigned to this specimen. */
  val inventoryId: String

  /** The [[domain.studies.CollectedSpecimenDefinition CollectedSpecimenDefinition]] this specimen
   * belongs to, defined by the study it belongs to. */
  val specimenDefinitionId: SpecimenDefinitionId

  /** The [[domain.centres.Centre Centre]] where this specimen was created. */
  val originLocationId: LocationId

  /** The [[domain.centres.Centre Centre]] where this specimen is currently located. */
  val locationId: LocationId

  /** The [[domain.containers.Container Container]] this specimen is stored in. */
  val containerId: Option[ContainerId]

  /**
   * The [[domain.containers.ContainerSchemaLabel ContainerSchemaLabel]] label this specimen has in its
   * container.
   */
  val schemaLabel: Option[ContainerSchemaLabel]

  /**
   * The date and time when the specimen was physically created.
   *
   * Not necessarily when this specimen was added to the application.
   */
  val timeCreated: OffsetDateTime

  /**
   * The amount, in units specified in the [[domain.studies.SpecimenDefinition SpecimenDefinition]], for this
   * specimen.
   */
  val amount: scala.math.BigDecimal

  override def toString: String = s"${this.getClass.getSimpleName}: ${Json.prettyPrint(Json.toJson(this))}"
}

object Specimen {
  val usableState:   EntityState = new EntityState("usable")
  val unusableState: EntityState = new EntityState("unusable")

  implicit val specimenFormat: Format[Specimen] = new Format[Specimen] {

    override def writes(specimen: Specimen): JsValue =
      ConcurrencySafeEntity.toJson(specimen) ++
        Json.obj("state"                -> specimen.state.id,
                 "slug"                 -> specimen.slug,
                 "inventoryId"          -> specimen.inventoryId,
                 "specimenDefinitionId" -> specimen.specimenDefinitionId,
                 "originLocationId"     -> specimen.originLocationId.id,
                 "locationId"           -> specimen.locationId.id,
                 "containerId"          -> specimen.containerId,
                 "schemaLabel"          -> specimen.schemaLabel,
                 "version"              -> specimen.version,
                 "timeCreated"          -> specimen.timeCreated,
                 "amount"               -> specimen.amount)

    override def reads(json: JsValue): JsResult[Specimen] = (json \ "state") match {
      case JsDefined(JsString(usableState.id))   => json.validate[UsableSpecimen]
      case JsDefined(JsString(unusableState.id)) => json.validate[UnusableSpecimen]
      case _                                     => JsError("error")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val usableSpecimenReads: Reads[UsableSpecimen] = Json.reads[UsableSpecimen]

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val unusableSpecimenReads: Reads[UnusableSpecimen] = Json.reads[UnusableSpecimen]

  val sort2Compare: Map[String, (Specimen, Specimen) => Boolean] =
    Map[String, (Specimen, Specimen) => Boolean]("inventoryId" -> compareByInventoryId,
                                                 "timeCreated" -> compareByTimeCreated,
                                                 "state"       -> compareByState)

  def compareById(a: Specimen, b: Specimen): Boolean =
    (a.id.id compareTo b.id.id) < 0

  def compareByInventoryId(a: Specimen, b: Specimen): Boolean =
    (a.inventoryId compareTo b.inventoryId) < 0

  def compareByTimeCreated(a: Specimen, b: Specimen): Boolean =
    (a.timeCreated compareTo b.timeCreated) < 0

  def compareByState(a: Specimen, b: Specimen): Boolean =
    (a.state.toString compareTo b.state.toString) < 0
}

trait SpecimenValidations {

  case object InventoryIdInvalid extends ValidationKey

  case object SpecimenDefinitionIdInvalid extends ValidationKey

}

/**
 * A usable specimen is a specimen that can be used for processing.
 */
final case class UsableSpecimen(
    id:                   SpecimenId,
    version:              Long,
    timeAdded:            OffsetDateTime,
    timeModified:         Option[OffsetDateTime],
    slug:                 Slug,
    inventoryId:          String,
    specimenDefinitionId: SpecimenDefinitionId,
    originLocationId:     LocationId,
    locationId:           LocationId,
    containerId:          Option[ContainerId],
    schemaLabel:          Option[ContainerSchemaLabel],
    timeCreated:          OffsetDateTime,
    amount:               BigDecimal)
    extends HasSlug with Specimen with SpecimenValidations with ParticipantValidations with StudyValidations {

  import org.biobank.domain.DomainValidations._
  import org.biobank.CommonValidations._

  val state: EntityState = Specimen.usableState

  def withInventoryId(inventoryId: String): DomainValidation[Specimen] =
    validateNonEmptyString(inventoryId, InventoryIdInvalid).map { s =>
      update.copy(slug = Slug(inventoryId), inventoryId = inventoryId)
    }

  def withAmount(amount: BigDecimal): DomainValidation[Specimen] =
    validatePositiveNumber(amount, AmountInvalid).map { s =>
      update.copy(amount = amount)
    }

  /**
   * Location should belong to a centre.
   */
  def withOrigin(id: LocationId): DomainValidation[Specimen] =
    validateId(id, LocationIdInvalid).map { s =>
      update.copy(originLocationId = id)
    }

  /**
   * Location should belong to a centre.
   */
  def withLocation(id: LocationId): DomainValidation[Specimen] =
    validateId(id, LocationIdInvalid).map { s =>
      update.copy(locationId = id)
    }

  def withContainerLabel(schemaLabel: Option[ContainerSchemaLabel]): DomainValidation[Specimen] =
    ContainerSchemaLabel.validate(schemaLabel).map { s =>
      update.copy(schemaLabel = schemaLabel)
    }

  def makeUnusable(): DomainValidation[UnusableSpecimen] =
    UnusableSpecimen(id                   = this.id,
                     version              = this.version + 1,
                     timeAdded            = this.timeAdded,
                     timeModified         = Some(OffsetDateTime.now),
                     slug                 = this.slug,
                     inventoryId          = this.inventoryId,
                     specimenDefinitionId = this.specimenDefinitionId,
                     originLocationId     = this.originLocationId,
                     locationId           = this.locationId,
                     containerId          = this.containerId,
                     schemaLabel          = this.schemaLabel,
                     timeCreated          = this.timeCreated,
                     amount               = this.amount).successNel[String]

  private def update() =
    copy(version = version + 1L, timeModified = Some(OffsetDateTime.now))
}

import org.biobank.domain.studies._

object UsableSpecimen extends SpecimenValidations with ParticipantValidations with StudyValidations {

  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  def create(
      id:                   SpecimenId,
      inventoryId:          String,
      specimenDefinitionId: SpecimenDefinitionId,
      version:              Long,
      originLocationId:     LocationId,
      locationId:           LocationId,
      containerId:          Option[ContainerId],
      schemaLabel:          Option[ContainerSchemaLabel],
      timeAdded:            OffsetDateTime,
      timeCreated:          OffsetDateTime,
      amount:               BigDecimal
    ): DomainValidation[UsableSpecimen] =
    validate(id,
             inventoryId,
             specimenDefinitionId,
             version,
             originLocationId,
             locationId,
             containerId,
             schemaLabel,
             amount)
      .map(
        _ =>
          UsableSpecimen(id                   = id,
                         version              = version,
                         timeAdded            = timeAdded,
                         timeModified         = None,
                         slug                 = Slug(inventoryId),
                         inventoryId          = inventoryId,
                         specimenDefinitionId = specimenDefinitionId,
                         originLocationId     = originLocationId,
                         locationId           = locationId,
                         containerId          = containerId,
                         schemaLabel          = schemaLabel,
                         timeCreated          = timeCreated,
                         amount               = amount)
      )

  def validate(
      id:                   SpecimenId,
      inventoryId:          String,
      specimenDefinitionId: SpecimenDefinitionId,
      version:              Long,
      originLocationId:     LocationId,
      locationId:           LocationId,
      containerId:          Option[ContainerId],
      schemaLabel:          Option[ContainerSchemaLabel],
      amount:               BigDecimal
    ): DomainValidation[Unit] =
    (validateId(id) |@|
      validateNonEmptyString(inventoryId, InventoryIdInvalid) |@|
      validateId(specimenDefinitionId, SpecimenDefinitionIdInvalid) |@|
      validateVersion(version) |@|
      validateNonEmptyString(originLocationId.id, OriginLocationIdInvalid) |@|
      validateNonEmptyString(locationId.id, LocationIdInvalid) |@|
      validateIdOption(containerId, ContainerIdInvalid) |@|
      ContainerSchemaLabel.validate(schemaLabel) |@|
      validatePositiveNumber(amount, AmountInvalid)) {
      case _ => ()
    }

}

/**
 * An Unusable specimen is a specimen that can no longer be used for processing.
 *
 * It may be that the total amount of the spcimen has already been used in processing.
 */
final case class UnusableSpecimen(
    id:                   SpecimenId,
    version:              Long,
    timeAdded:            OffsetDateTime,
    timeModified:         Option[OffsetDateTime],
    slug:                 Slug,
    inventoryId:          String,
    specimenDefinitionId: SpecimenDefinitionId,
    originLocationId:     LocationId,
    locationId:           LocationId,
    containerId:          Option[ContainerId],
    schemaLabel:          Option[ContainerSchemaLabel],
    timeCreated:          OffsetDateTime,
    amount:               BigDecimal)
    extends HasSlug with Specimen {

  val state: EntityState = Specimen.unusableState

  def makeUsable(): DomainValidation[UsableSpecimen] =
    UsableSpecimen(id                   = this.id,
                   version              = this.version + 1,
                   timeAdded            = this.timeAdded,
                   timeModified         = Some(OffsetDateTime.now),
                   slug                 = this.slug,
                   inventoryId          = this.inventoryId,
                   specimenDefinitionId = this.specimenDefinitionId,
                   originLocationId     = this.originLocationId,
                   locationId           = this.locationId,
                   containerId          = this.containerId,
                   schemaLabel          = this.schemaLabel,
                   timeCreated          = this.timeCreated,
                   amount               = this.amount).successNel[String]
}
