package org.biobank.domain.studies

//import com.github.ghik.silencer.silent
import org.biobank.ValidationKey
import org.biobank.domain._
import org.biobank.domain.AnatomicalSourceType._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.PreservationType._
import org.biobank.domain.SpecimenType._
import org.biobank.domain.{DomainValidation, HasOptionalDescription, HasUniqueName}
import play.api.libs.json._
import scalaz.Scalaz._

/** Identifies a unique [[SpecimenDefinition]] in a Collection Event Type.
 *
 * Used as a value object to maintain associations to with entities in the system.
 */
final case class SpecimenDefinitionId(id: String) extends IdentifiedValueObject[String]

object SpecimenDefinitionId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val specimenDefinitionIdFormat: Format[SpecimenDefinitionId] = new Format[SpecimenDefinitionId] {

    override def writes(id: SpecimenDefinitionId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[SpecimenDefinitionId] =
      Reads.StringReads.reads(json).map(SpecimenDefinitionId.apply _)
  }

}

/**
 * Used to define the [[domain.participants.Specimen Specimens]] collected or processed by a [[Study]].
 *
 * It records ownership, summary, storage, and classification information that applies to an entire group or
 * collection of [[domain.participants.Specimen Specimens]]. A specimen description is defined either for
 * specimen types collected from participants, or for specimen types that are processed.
 */
trait SpecimenDefinition
    extends IdentifiedValueObject[SpecimenDefinitionId] with HasUniqueName with HasSlug
    with HasOptionalDescription {

  /** A short identifying name that is unique to the study. */
  val name: String

  val description: Option[String]

  /** Specifies how the specimen amount is measured (e.g. volume, weight, length, etc.). */
  val units: String

  /** See [[AnatomicalSourceType]]. */
  val anatomicalSourceType: AnatomicalSourceType

  /** See [[PreservationType]]. */
  val preservationType: PreservationType

  /** See [[PreservationType]]. */
  val preservationTemperature: PreservationTemperature

  /** See [[SpecimenType]]. */
  val specimenType: SpecimenType

  override def toString: String =
    s"""|SpecimenDefinition:{
        |  id:                      $id,
        |  slug:                    $slug,
        |  name:                    $name,
        |  description:             $description,
        |  units:                   $units,
        |  anatomicalSourceType:    $anatomicalSourceType,
        |  preservationType:        $preservationType,
        |  preservationTemperature: $preservationTemperature,
        |  specimenType:            $specimenType
        |}""".stripMargin

}

trait SpecimenDefinitionValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  case object UnitsRequired extends ValidationKey

  case object MaxCountInvalid extends ValidationKey

  case object AmountInvalid extends ValidationKey

  /**
   * The factory method to create a specimen group. Note that it increments the version number
   * by one.
   *
   * Performs validation on fields.
   *
   * @param version the previous version number for the specimen group. If the specimen group is
   * new then this value should be 0L.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(name: String, description: Option[String], units: String): DomainValidation[Unit] =
    (validateNonEmptyString(name, NameRequired) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateString(units, UnitsRequired)) {
      case _ => ()
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(specimenDefinition: SpecimenDefinition): DomainValidation[Unit] =
    validate(specimenDefinition.name, specimenDefinition.description, specimenDefinition.units)

}

/**
 * Used to define a [[domain.participants.Specimen Specimen]] that is collected by a [[domain.studies.Study
 * Study]].
 *
 * It records ownership, summary, storage, and classification information that applies to an entire group or
 * collection of [[domain.participants. Specimen Specimens]]. A specimen description is defined either for
 * specimen types collected from participants, or for specimen types that are processed.
 */
final case class CollectedSpecimenDefinition(
    id:                      SpecimenDefinitionId,
    slug:                    Slug,
    name:                    String,
    description:             Option[String],
    units:                   String,
    anatomicalSourceType:    AnatomicalSourceType,
    preservationType:        PreservationType,
    preservationTemperature: PreservationTemperature,
    specimenType:            SpecimenType,
    maxCount:                Int,
    amount:                  BigDecimal)
    extends SpecimenDefinition

object CollectedSpecimenDefinition extends SpecimenDefinitionValidations {
  import org.biobank.CommonValidations._

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val collectedSpecimenDefinitionFormat: Format[CollectedSpecimenDefinition] =
    Json.format[CollectedSpecimenDefinition]

  val hashidsSalt: String = "biobank-collection-event-types"

  /**
   * Creates a [[domain.studies.CollectedSpecimenDefinition.create CollectedSpecimenDefinition]] with the
   * given properties.
   */
  def create(
      name:                    String,
      description:             Option[String],
      units:                   String,
      anatomicalSourceType:    AnatomicalSourceType,
      preservationType:        PreservationType,
      preservationTemperature: PreservationTemperature,
      specimenType:            SpecimenType,
      maxCount:                Int,
      amount:                  BigDecimal
    ): DomainValidation[CollectedSpecimenDefinition] =
    validate(name,
             description,
             units,
             anatomicalSourceType,
             preservationType,
             preservationTemperature,
             specimenType,
             maxCount,
             amount).map { _ =>
      val id = SpecimenDefinitionId(java.util.UUID.randomUUID.toString.replaceAll("-", "").toUpperCase)
      CollectedSpecimenDefinition(id                      = id,
                                  slug                    = Slug(name),
                                  name                    = name,
                                  description             = description,
                                  units                   = units,
                                  anatomicalSourceType    = anatomicalSourceType,
                                  preservationType        = preservationType,
                                  preservationTemperature = preservationTemperature,
                                  specimenType            = specimenType,
                                  maxCount                = maxCount,
                                  amount                  = amount)
    }

  //@silent
  def validate(
      name:                    String,
      description:             Option[String],
      units:                   String,
      anatomicalSourceType:    AnatomicalSourceType,
      preservationType:        PreservationType,
      preservationTemperature: PreservationTemperature,
      specimenType:            SpecimenType,
      maxCount:                Int,
      amount:                  BigDecimal
    ): DomainValidation[Unit] =
    (validate(name, description, units) |@|
      validatePositiveNumber(maxCount, MaxCountInvalid) |@|
      validatePositiveNumber(amount, AmountInvalid)) {
      case _ => ()
    }

}

final case class ProcessedSpecimenDefinition(
    id:                      SpecimenDefinitionId,
    slug:                    Slug,
    name:                    String,
    description:             Option[String],
    units:                   String,
    anatomicalSourceType:    AnatomicalSourceType,
    preservationType:        PreservationType,
    preservationTemperature: PreservationTemperature,
    specimenType:            SpecimenType)
    extends SpecimenDefinition

object ProcessedSpecimenDefinition extends SpecimenDefinitionValidations {

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val processingSpecimenSpecFormat: Format[ProcessedSpecimenDefinition] =
    Json.format[ProcessedSpecimenDefinition]

  def create(
      name:                    String,
      description:             Option[String],
      units:                   String,
      anatomicalSourceType:    AnatomicalSourceType,
      preservationType:        PreservationType,
      preservationTemperature: PreservationTemperature,
      specimenType:            SpecimenType
    ): DomainValidation[ProcessedSpecimenDefinition] =
    validate(name,
             description,
             units,
             anatomicalSourceType,
             preservationType,
             preservationTemperature,
             specimenType).map { _ =>
      val id = SpecimenDefinitionId(java.util.UUID.randomUUID.toString.replaceAll("-", "").toUpperCase)
      ProcessedSpecimenDefinition(id                      = id,
                                  slug                    = Slug(name),
                                  name                    = name,
                                  description             = description,
                                  units                   = units,
                                  anatomicalSourceType    = anatomicalSourceType,
                                  preservationType        = preservationType,
                                  preservationTemperature = preservationTemperature,
                                  specimenType            = specimenType)
    }

  //@silent
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(
      name:                    String,
      description:             Option[String],
      units:                   String,
      anatomicalSourceType:    AnatomicalSourceType,
      preservationType:        PreservationType,
      preservationTemperature: PreservationTemperature,
      specimenType:            SpecimenType
    ): DomainValidation[Unit] =
    validate(name, description, units).map { _ =>
      ()
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(specimenDefinition: ProcessedSpecimenDefinition): DomainValidation[Unit] =
    validate(specimenDefinition.name,
             specimenDefinition.description,
             specimenDefinition.units,
             specimenDefinition.anatomicalSourceType,
             specimenDefinition.preservationType,
             specimenDefinition.preservationTemperature,
             specimenDefinition.specimenType)

}
