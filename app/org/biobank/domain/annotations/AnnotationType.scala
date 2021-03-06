package org.biobank.domain.annotations

import org.biobank._
import org.biobank.domain._
import org.biobank.domain.annotations.AnnotationValueType._
import play.api.libs.json._
import scalaz.Scalaz._

/** Identifies a unique [[domain.annotations.AnnotationType AnnotationType]] in a [[domain.studies.Study
 * Study]] or [[domain.studies.CollectionEventType CollectionEventType]].
 *
 * Used as a value object to maintain associations to with entities in the system.
 */
final case class AnnotationTypeId(id: String) extends IdentifiedValueObject[String]

object AnnotationTypeId {

  // Do not want JSON to create a sub object, we just want it to be converted
  // to a single string
  implicit val annotationTypeIdFormat: Format[AnnotationTypeId] = new Format[AnnotationTypeId] {

    override def writes(id: AnnotationTypeId): JsValue = JsString(id.id)

    override def reads(json: JsValue): JsResult[AnnotationTypeId] =
      Reads.StringReads.reads(json).map(AnnotationTypeId.apply _)
  }

}

/**
 * Annotation types define an [[Annotation]].
 *
 * Annotations allow entities to collect custom named and defined pieces of data.
 *
 * This is a value object.
 *
 * @param valueType The type of information stored by the annotation. I.e. text, number, date, or an
 * item from a drop down list. See [[AnnotationValueType]].
 *
 * @param maxValueCount When valueType is [[AnnotationValueType.Select]] (i.e. a drop down list), this is the
 * number of items allowed to be selected. If the value is greater than 1 then any number of values can be
 * selected.
 *
 * @param options When valueType is [[AnnotationValueType.Select]], these are the list of options allowed to
 * be selected.
 *
 * @param required When true, the user must enter a value for this annotation.
 */
final case class AnnotationType(
    id:            AnnotationTypeId,
    slug:          Slug,
    name:          String,
    description:   Option[String],
    valueType:     AnnotationValueType,
    maxValueCount: Option[Int],
    options:       Seq[String],
    required:      Boolean)
    extends IdentifiedValueObject[AnnotationTypeId] with HasName with HasOptionalDescription
    with AnnotationTypeValidations {

  override def toString: String =
    s"""|AnnotationType:{
        |  id:            $id,
        |  slug:          $slug,
        |  name:          $name,
        |  description:   $description,
        |  valueType:     $valueType,
        |  maxValueCount: $maxValueCount,
        |  options:       { $options },
        |  required:      $required
        |}""".stripMargin

}

trait AnnotationTypeValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  case object MaxValueCountError extends ValidationKey

  case object OptionRequired extends ValidationKey

  case object DuplicateOptionsError extends ValidationKey

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(
      name:          String,
      description:   Option[String],
      valueType:     AnnotationValueType,
      maxValueCount: Option[Int],
      options:       Seq[String]
    ): DomainValidation[Unit] =
    (validateString(name, NameRequired) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateMaxValueCount(maxValueCount) |@|
      validateOptions(options) |@|
      validateSelectParams(valueType, maxValueCount, options)) {
      case _ => ()
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(annotationType: AnnotationType): DomainValidation[Unit] =
    validate(annotationType.name,
             annotationType.description,
             annotationType.valueType,
             annotationType.maxValueCount,
             annotationType.options)

  def validateMaxValueCount(option: Option[Int]): DomainValidation[Option[Int]] =
    option match {
      case None => option.successNel[String]
      case Some(n) =>
        if (n > -1) {
          option.successNel[String]
        } else {
          MaxValueCountError.failureNel[Option[Int]]
        }
    }

  /**
   *  Validates each item in the map and returns all failures.
   */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def validateOptions(options: Seq[String]): DomainValidation[Seq[String]] =
    if (options.distinct.size === options.size) {
      options.toList
        .map(validateNonEmptyString(_, OptionRequired)).sequenceU.fold(
          err  => err.toList.mkString(",").failureNel[Seq[String]],
          list => list.toSeq.successNel
        )
    } else {
      DuplicateOptionsError.failureNel[Seq[String]]
    }

  /** If an annotation type is for a select, the following is required:
   *
   * - max value count must be 1 or 2
   * - options must be a non-empty sequence
   *
   * If an annotation type is not for a select, the following is required
   *
   * - max value count must be 0
   * - options must be None
   */
  def validateSelectParams(
      valueType:     AnnotationValueType,
      maxValueCount: Option[Int],
      options:       Seq[String]
    ): DomainValidation[Unit] =
    if (valueType == AnnotationValueType.Select) {
      maxValueCount.fold {
        DomainError(s"max value count is invalid for select").failureNel[Unit]
      } { count =>
        val countValidation: DomainValidation[Unit] = if ((count < 1) || (count > 2)) {
          DomainError(s"select annotation type with invalid maxValueCount: $count").failureNel[Unit]
        } else {
          ().successNel[String]
        }

        val optionsValidation = if (options.isEmpty) {
          DomainError("select annotation type with no options to select").failureNel[Unit]
        } else {
          ().successNel[String]
        }

        (countValidation |@| optionsValidation) { case _ => () }
      }
    } else {
      val countValidation: DomainValidation[Unit] = maxValueCount.fold {
        ().successNel[String]
      } { count =>
        DomainError(s"max value count is invalid for non-select").failureNel[Unit]
      }

      val optionsValidation = if (options.isEmpty) {
        ().successNel[String]
      } else {
        DomainError("non select annotation type with options to select").failureNel[Unit]
      }

      (countValidation |@| optionsValidation) {
        case _ => ()
      }
    }

}

object AnnotationType extends AnnotationTypeValidations {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val annotationTypeFormat: Format[AnnotationType] = Json.format[AnnotationType]

  def create(
      name:          String,
      description:   Option[String],
      valueType:     AnnotationValueType,
      maxValueCount: Option[Int],
      options:       Seq[String],
      required:      Boolean
    ): DomainValidation[AnnotationType] =
    (validateNonEmptyString(name, NameRequired) |@|
      validateNonEmptyStringOption(description, InvalidDescription) |@|
      validateMaxValueCount(maxValueCount) |@|
      validateOptions(options) |@|
      validateSelectParams(valueType, maxValueCount, options)) {
      case _ =>
        val id = AnnotationTypeId(java.util.UUID.randomUUID.toString.replaceAll("-", "").toUpperCase)
        AnnotationType(id            = id,
                       slug          = Slug(name),
                       name          = name,
                       description   = description,
                       valueType     = valueType,
                       maxValueCount = maxValueCount,
                       options       = options,
                       required      = required)
    }

}
