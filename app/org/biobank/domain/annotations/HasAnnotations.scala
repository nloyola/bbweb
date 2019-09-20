package org.biobank.domain.annotations

import org.biobank.domain._
import scalaz.Scalaz._

trait HasAnnotations[T <: ConcurrencySafeEntity[_]] {
  import org.biobank.CommonValidations._

  val annotations: Set[Annotation]

  def withAnnotation(annotation: Annotation): DomainValidation[T]

  def withoutAnnotation(annotationTypeId: AnnotationTypeId): DomainValidation[T]

  /** removes an annotation type. */
  protected def checkRemoveAnnotation(annotationTypeId: AnnotationTypeId): DomainValidation[Annotation] =
    annotations
      .find { x =>
        x.annotationTypeId == annotationTypeId
      }
      .toSuccessNel(IdNotFound(s"annotation type ID: $annotationTypeId").toString)

}
