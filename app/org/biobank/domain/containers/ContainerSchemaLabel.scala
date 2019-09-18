package org.biobank.domain.containers

import org.biobank.ValidationKey
import org.biobank.domain._
import play.api.libs.json._
import scalaz.Scalaz._
import scalaz.NonEmptyList._

object ContainerSchemaLabelValidations {

  case object InvalidContainerSchemaId extends ValidationKey

  case object InvalidLabel extends ValidationKey

  case object ContainerSchemaLabelInvalid extends ValidationKey

}

trait HasContainerSchemaLabel {

  val schemaLabel: Option[ContainerSchemaLabel]
}

/**
 * Represents a labelled position that a child (e.g. a [[Container]] or a [[domain.participants.Specimen
 * Specimen]]) has in a parent [[Container]]. Labels are associated with a single [[ContainerSchema]].
 *
 * This is a value object because it must be referenced and the [[label]] could be quite long.
 */
final case class ContainerSchemaLabel(schemaId: ContainerSchemaId, label: String) {

  override def toString: String = Json.prettyPrint(Json.toJson(this))

}

object ContainerSchemaLabel {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._
  import ContainerSchemaLabelValidations._

  def create(schemaId: ContainerSchemaId, label: String): DomainValidation[ContainerSchemaLabel] =
    validate(schemaId, label) map { _ =>
      ContainerSchemaLabel(schemaId, label)
    }

  def validate(schemaId: ContainerSchemaId, label: String): DomainValidation[Unit] =
    (validateId(schemaId, InvalidContainerSchemaId) |@|
      validateNonEmptyString(label, ContainerSchemaLabelInvalid)) {
      case _ =>
        ()
    }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(schemaLabel: ContainerSchemaLabel): DomainValidation[Unit] =
    ContainerSchemaLabel.validate(schemaLabel.schemaId, schemaLabel.label)

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(schemaLabel: Option[ContainerSchemaLabel]): DomainValidation[Unit] =
    schemaLabel match {
      case Some(l) => ContainerSchemaLabel.validate(l.schemaId, l.label)
      case None    => ().successNel[String]
    }

  implicit val containerSchemLabelFormat: Format[ContainerSchemaLabel] =
    Json.format[ContainerSchemaLabel]

}
