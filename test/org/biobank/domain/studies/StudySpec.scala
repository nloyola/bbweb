package org.biobank.domain.studies

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.domain.annotations._
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory

class StudySpec extends DomainSpec with AnnotationTypeSetSharedSpec[DisabledStudy] {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._
  import org.scalatest.matchers.must.Matchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(study: Study): DomainValidation[DisabledStudy] =
    DisabledStudy.create(study.id, study.version, study.name, study.description, study.annotationTypes)

  describe("A study") {

    it("be created") {
      val study = factory.createDisabledStudy
      createFrom(study).mustSucceed {
        _ must matchStudy(study)
      }
    }

    it("have it's name updated") {
      val study = factory.createDisabledStudy
      val name  = nameGenerator.next[Study]

      study.withName(name) mustSucceed { actual =>
        actual.id must be(study.id)
        actual.version must be(study.version + 1L)
        actual.name must be(name)
        actual.description must be(study.description)
        actual.annotationTypes mustBe empty

        actual must beEntityWithTimeStamps(study.timeAdded, Some(OffsetDateTime.now), 5L)
      }
    }

    it("have it's description updated") {
      val study       = factory.createDisabledStudy
      val description = Some(nameGenerator.next[Study])

      study.withDescription(description) mustSucceed { actual =>
        actual.id must be(study.id)
        actual.version must be(study.version + 1L)
        actual.name must be(study.name)
        actual.description must be(description)

        actual must beEntityWithTimeStamps(study.timeAdded, Some(OffsetDateTime.now), 5L)
      }
    }

    it("be enabled") {
      val study = factory.createDisabledStudy
      study.enable mustSucceed { enabledStudy =>
        enabledStudy mustBe a[EnabledStudy]
        enabledStudy.timeAdded mustBe (study.timeAdded)
        ()
      }
    }

    it("when disabled, can be enabled") {
      val study = factory.createEnabledStudy
      study.disable mustSucceed { disabledStudy =>
        disabledStudy mustBe a[DisabledStudy]
        disabledStudy.timeAdded mustBe (study.timeAdded)
        ()
      }
    }

    it("be retired") {
      val study = factory.createDisabledStudy
      study.retire mustSucceed { retiredStudy =>
        retiredStudy mustBe a[RetiredStudy]
        retiredStudy.timeAdded mustBe (study.timeAdded)
        ()
      }
    }

    it("be unretired") {
      val study = factory.createRetiredStudy
      study.unretire() mustSucceed { disabledStudy =>
        disabledStudy mustBe a[DisabledStudy]
        disabledStudy.timeAdded mustBe (study.timeAdded)
        ()
      }
    }

  }

  describe("A study") {

    it("not be created with an empty id") {
      val study = factory.createDisabledStudy.copy(id = StudyId(""))
      createFrom(study) mustFail "IdRequired"
    }

    it("not be created with an invalid version") {
      val study = factory.createDisabledStudy.copy(version = -2L)
      createFrom(study) mustFail "InvalidVersion"
    }

    it("not be created with an null or empty name") {
      var study = factory.createDisabledStudy.copy(name = null)
      createFrom(study) mustFail "InvalidName"

      study = factory.createDisabledStudy.copy(name = "")
      createFrom(study) mustFail "InvalidName"
    }

    it("not be created with an empty description") {
      var study = factory.createDisabledStudy.copy(description = Some(null))
      createFrom(study) mustFail "InvalidDescription"

      study = factory.createDisabledStudy.copy(description = Some(""))
      createFrom(study) mustFail "InvalidDescription"
    }

    it("have more than one validation fail") {
      val study = factory.createDisabledStudy.copy(version = -2L, name = "")
      createFrom(study) mustFail ("InvalidVersion", "InvalidName")
    }

  }

  override def createEntity(): DisabledStudy =
    factory.createDisabledStudy.copy(annotationTypes = Set.empty)

  override def getAnnotationTypeSet(study: DisabledStudy): Set[AnnotationType] =
    study.annotationTypes

  override def addAnnotationType(
      study:          DisabledStudy,
      annotationType: AnnotationType
    ): DomainValidation[DisabledStudy] =
    study.withParticipantAnnotationType(annotationType)

  override def removeAnnotationType(
      study: DisabledStudy,
      id:    AnnotationTypeId
    ): DomainValidation[DisabledStudy] =
    study.removeParticipantAnnotationType(id)

  describe("A study's annotation type set") {

    annotationTypeSetSharedBehaviour

  }

}
