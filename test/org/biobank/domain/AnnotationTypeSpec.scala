package org.biobank.domain

import org.biobank.domain.annotations._
import org.biobank.fixtures.NameGenerator

class AnnotationTypeSpec extends DomainSpec {
  import org.biobank.TestUtils._
  import AnnotationTypeSpecUtil._

  val nameGenerator = new NameGenerator(this.getClass)

  def createAnnotationType(tuple: AnnotTypeTuple): DomainValidation[AnnotationType] =
    (AnnotationType.create _).tupled(tuple)

  describe("An Annotation Type") {

    it("be created for each value type") {
      AnnotationValueType.values.foreach { vt =>
        val tuple = AnnotationValueTypeToTuple(vt)
        createAnnotationType(tuple).mustSucceed { annotType =>
          annotType.id.id must not be empty
          annotType.name must be(tuple._1)
          annotType.description must be(tuple._2)
          annotType.valueType must be(tuple._3)
          annotType.maxValueCount must be(tuple._4)
          annotType.options must be(tuple._5)
          annotType.required must be(tuple._6)
          ()
        }
      }
    }

    it("not be created with an null or empty name") {
      val invalidNames: List[String] = List(null, "")

      invalidNames.foreach { invalidName =>
        createAnnotationType(numberAnnotationTypeTuple.copy(_1 = invalidName))
          .mustFail("NameRequired")
      }
    }

    it("not be created with an empty description option") {
      val invalidDescriptions = List(Some(null), Some(""))

      invalidDescriptions.foreach { invalidDescription =>
        createAnnotationType(numberAnnotationTypeTuple.copy(_2 = invalidDescription))
          .mustFail("InvalidDescription")
      }
    }

    it("not be created with an negative max value count") {
      createAnnotationType(numberAnnotationTypeTuple.copy(_4 = Some(-1)))
        .mustFail(1, "MaxValueCountError")
    }

    it("not be created with an invalid options") {
      val annotationType = numberAnnotationTypeTuple.copy(_5 = Seq(""))
      createAnnotationType(annotationType) mustFail (1, "OptionRequired")

      createAnnotationType(numberAnnotationTypeTuple.copy(_5 = Seq("dup", "dup")))
        .mustFail(1, "DuplicateOptionsError")
    }

    it("have more than one validation fail") {
      createAnnotationType(numberAnnotationTypeTuple.copy(_1 = "", _5 = Seq("dup", "dup")))
        .mustFail("NameRequired",
                  "DuplicateOptionsError",
                  "non select annotation type with options to select")
    }

  }
}
