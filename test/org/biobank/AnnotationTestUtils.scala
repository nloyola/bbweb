package org.biobank

import org.biobank.domain.annotations.Annotation

import play.api.libs.json._

/**
 * This code is supposed to be independent of the application code, so Json objects are created from scratch.
 */
object AnnotationTestUtils {

  /** Converts a participant annotation into a Json object.
   */
  def annotationToJson(annotation: Annotation) =
    Json.obj("annotationTypeId" -> annotation.annotationTypeId,
             "valueType"        -> annotation.valueType,
             "stringValue"      -> annotation.stringValue,
             "numberValue"      -> annotation.numberValue,
             "selectedValues"   -> annotation.selectedValues)

}
