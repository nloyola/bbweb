package org.biobank.domain.containers

import org.biobank.ValidationKey
import org.biobank.domain._
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.NonEmptyList._

object ContainerSchemaPositionValidations {

  case object InvalidContainerSchemaId extends ValidationKey

  case object InvalidLabel extends ValidationKey

  case object ContainerSchemaPositionInvalid extends ValidationKey

}

/**
 * Represents a labelled position that a child (e.g. a [[Container]] or a [[domain.participants.Specimen
 * Specimen]]) has in a parent [[Container]]. Labels are associated with a single [[ContainerSchema]].
 *
 * This is a value object because it must be referenced and the [[label]] could be quite long.
 */
final case class ContainerSchemaPosition(schemaId: ContainerSchemaId, label: String) {

  override def toString: String =
    s"""|${this.getClass.getSimpleName}: {
        |  schemaId: $schemaId,
        |  label:    $label
        |}""".stripMargin

}

object ContainerSchemaPosition {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._
  import ContainerSchemaPositionValidations._

  def create(schemaId: ContainerSchemaId, label: String): DomainValidation[ContainerSchemaPosition] =
    validate(schemaId, label) map { _ =>
      ContainerSchemaPosition(schemaId, label)
    }

  def validate(schemaId: ContainerSchemaId, label: String): DomainValidation[Unit] =
    (validateId(schemaId, InvalidContainerSchemaId) |@|
      validateNonEmptyString(label, InvalidLabel)) {
      case _ =>
        ()
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(position: ContainerSchemaPosition): DomainValidation[Unit] =
    ContainerSchemaPosition.validate(position.schemaId, position.label)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(position: Option[ContainerSchemaPosition]): DomainValidation[Unit] =
    position match {
      case Some(p) => ContainerSchemaPosition.validate(p.schemaId, p.label)
      case None    => ().successNel[String]
    }

  implicit val containerSchemPositionFormat: Format[ContainerSchemaPosition] =
    Json.format[ContainerSchemaPosition]

}
