package org.biobank.domain.annotations

import org.biobank.domain._
import scalaz.Scalaz._

trait HasAnnotationTypes extends AnnotationTypeValidations {

  import org.biobank.CommonValidations._

  val annotationTypes: Set[AnnotationType]

  protected def checkAddAnnotationType(annotationType: AnnotationType): DomainValidation[Unit] =
    (validate(annotationType) |@| nameNotUsed(annotationType)) {
      case _ => ()
    }

  protected def checkRemoveAnnotationType(
      annotationTypeId: AnnotationTypeId
    ): DomainValidation[AnnotationType] =
    annotationTypes
      .find { x =>
        x.id == annotationTypeId
      }
      .toSuccessNel(IdNotFound(s"annotation type ID: $annotationTypeId").toString)

  protected def nameNotUsed(annotationType: AnnotationType): DomainValidation[Unit] = {
    val nameLowerCase = annotationType.name.toLowerCase
    annotationTypes
      .find { x =>
        (x.name.toLowerCase == nameLowerCase) && (x.id != annotationType.id)
      } match {
      case Some(_) =>
        EntityCriteriaError(s"annotation type name already used: ${annotationType.name}").failureNel[Unit]
      case None =>
        ().successNel[DomainError]
    }
  }

}
