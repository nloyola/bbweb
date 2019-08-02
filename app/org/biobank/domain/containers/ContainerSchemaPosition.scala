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

  def validate(position: Option[ContainerSchemaPosition]): DomainValidation[Unit] = {
    ContainerSchemaPosition.validate(position).leftMap { err =>
      nel(ContainerSchemaPositionValidations.ContainerSchemaPositionInvalid.toString, err.list)
    }
  }

}

trait HasContainerSchemaPosition {

  val position: Option[ContainerSchemaPosition]
}

/**
 * Represents a labelled position that a child (e.g. a [[Container]] or a [[domain.participants.Specimen
 * Specimen]]) has in a parent [[Container]]. Labels are associated with a single [[ContainerSchema]].
 *
 * This is a value object because it must be referenced and the [[label]] could be quite long.
 */
final case class ContainerSchemaPosition(id:       ContainerSchemaPositionId,
                                         schemaId: ContainerSchemaId,
                                         label:    String)
    extends IdentifiedDomainObject[ContainerSchemaPositionId] {

  override def toString: String =  {
    s"""|${this.getClass.getSimpleName}: {
        |  id:       $id,
        |  schemaId: $schemaId,
        |  label:    $label
        |}""".stripMargin
  }


}

object ContainerSchemaPosition {
  import org.biobank.CommonValidations._
  import org.biobank.domain.DomainValidations._
  import ContainerSchemaPositionValidations._

  def create(id: ContainerSchemaPositionId, schemaId: ContainerSchemaId, label: String)
      : DomainValidation[ContainerSchemaPosition] = {
    validate(id, schemaId, label) map { _ =>
        ContainerSchemaPosition(id, schemaId, label)
    }
  }

  def validate(id: ContainerSchemaPositionId, schemaId: ContainerSchemaId, label: String)
      : DomainValidation[Unit] = {
    (validateId(id) |@|
       validateId(schemaId, InvalidContainerSchemaId) |@|
       validateNonEmptyString(label, InvalidLabel)) { case _ =>
        ()
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def validate(position: Option[ContainerSchemaPosition]): DomainValidation[Unit] = {
    position match {
      case Some(p) => ContainerSchemaPosition.validate(p.id, p.schemaId, p.label)
      case None => ().successNel[String]
    }
  }

  implicit val containerSchemPositionFormat: Format[ContainerSchemaPosition] =
    Json.format[ContainerSchemaPosition]

}
