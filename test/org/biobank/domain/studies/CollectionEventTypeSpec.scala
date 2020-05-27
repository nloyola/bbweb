package org.biobank.domain.studies

import java.time.OffsetDateTime
import org.biobank.domain._
import org.biobank.domain.annotations._
import org.biobank.fixtures.NameGenerator
import org.slf4j.LoggerFactory
import org.scalatest.prop.TableDrivenPropertyChecks._

class CollectionEventTypeSpec extends DomainSpec with AnnotationTypeSetSharedSpec[CollectionEventType] {
  import org.biobank.TestUtils._
  import org.biobank.matchers.EntityMatchers._
  import org.scalatest.matchers.must.Matchers._

  val log = LoggerFactory.getLogger(this.getClass)

  val nameGenerator = new NameGenerator(this.getClass)

  def createFrom(eventType: CollectionEventType): DomainValidation[CollectionEventType] =
    CollectionEventType.create(eventType.studyId,
                               eventType.id,
                               eventType.version,
                               eventType.name,
                               eventType.description,
                               eventType.recurring,
                               eventType.specimenDefinitions,
                               eventType.annotationTypes)

  describe("A collection event type can") {

    it("be created") {
      val eventType = factory.createCollectionEventType
      createFrom(eventType) mustSucceed { actual =>
        actual mustBe a[CollectionEventType]

        actual.studyId must be(eventType.studyId)
        actual.id must be(eventType.id)
        actual.version must be(eventType.version)
        actual.name must be(eventType.name)
        actual.description must be(eventType.description)
        actual.recurring must be(eventType.recurring)

        actual.specimenDefinitions must have size 0
        actual.annotationTypes must have size 0

        actual must beEntityWithTimeStamps(OffsetDateTime.now, None, 5L)
      }
    }

    it("have it's name updated") {
      val eventType = factory.createCollectionEventType
      val name      = nameGenerator.next[CollectionEventType]

      eventType.withName(name) mustSucceed { actual =>
        actual mustBe a[CollectionEventType]

        actual.studyId must be(eventType.studyId)
        actual.id must be(eventType.id)
        actual.version must be(eventType.version + 1L)
        actual.name must be(name)
        actual.description must be(eventType.description)
        actual.recurring must be(eventType.recurring)

        actual.specimenDefinitions must have size 0
        actual.annotationTypes must have size 0

        actual must beEntityWithTimeStamps(eventType.timeAdded, Some(OffsetDateTime.now), 5L)
      }
    }

    it("have it's description updated") {
      val eventType   = factory.createCollectionEventType
      val description = Some(nameGenerator.next[CollectionEventType])

      eventType.withDescription(description) mustSucceed { actual =>
        actual mustBe a[CollectionEventType]

        actual.studyId must be(eventType.studyId)
        actual.id must be(eventType.id)
        actual.version must be(eventType.version + 1L)
        actual.name must be(eventType.name)
        actual.description must be(description)
        actual.recurring must be(eventType.recurring)

        actual.specimenDefinitions must have size 0
        actual.annotationTypes must have size 0

        actual must beEntityWithTimeStamps(eventType.timeAdded, Some(OffsetDateTime.now), 5L)
      }
    }

    it("have it's recurring field updated") {
      val eventType      = factory.createCollectionEventType
      val recurringTable = Table(("possible recurringg values"), (true), (false))

      forAll(recurringTable) { recurring =>
        eventType.withRecurring(recurring) mustSucceed { actual =>
          actual mustBe a[CollectionEventType]

          actual.studyId must be(eventType.studyId)
          actual.id must be(eventType.id)
          actual.version must be(eventType.version + 1L)
          actual.name must be(eventType.name)
          actual.description must be(eventType.description)
          actual.recurring must be(recurring)

          actual.specimenDefinitions must have size 0
          actual.annotationTypes must have size 0

          actual must beEntityWithTimeStamps(eventType.timeAdded, Some(OffsetDateTime.now), 5L)
        }
      }
    }

  }

  describe("A collection event type") {

    it("not be created with an empty study id") {
      val eventType = factory.createCollectionEventType.copy(studyId = StudyId(""))
      createFrom(eventType) mustFail "StudyIdRequired"
    }

    it("not be created with an empty id") {
      val eventType = factory.createCollectionEventType.copy(id = CollectionEventTypeId(""))
      createFrom(eventType) mustFail "IdRequired"
    }

    it("not be created with an invalid version") {
      val eventType = factory.createCollectionEventType.copy(version = -2L)
      createFrom(eventType) mustFail "InvalidVersion"
    }

    it("not be created with an null or empty name") {
      var eventType = factory.createCollectionEventType.copy(name = null)
      createFrom(eventType) mustFail "NameRequired"

      eventType = factory.createCollectionEventType.copy(name = "")
      createFrom(eventType) mustFail "NameRequired"
    }

    it("not be created with an empty description option") {
      var eventType = factory.createCollectionEventType.copy(description = Some(null))
      createFrom(eventType) mustFail "InvalidDescription"

      eventType = factory.createCollectionEventType.copy(description = Some(""))
      createFrom(eventType) mustFail "InvalidDescription"
    }

    it("have more than one validation fail") {
      val eventType = factory.createCollectionEventType.copy(version = -2L, name = "")
      createFrom(eventType) mustFail ("InvalidVersion", "NameRequired")
    }

  }

  describe("A collection event type's specimen spec set") {

    it("add a specimen spec") {
      val eventType          = factory.createCollectionEventType.copy(specimenDefinitions = Set.empty)
      val specimenDefinition = factory.createCollectedSpecimenDefinition

      eventType.withSpecimenDefinition(specimenDefinition) mustSucceed { actual =>
        actual.studyId must be(eventType.studyId)
        actual.id must be(eventType.id)
        actual.version must be(eventType.version + 1)
        actual.name must be(eventType.name)
        actual.description must be(eventType.description)
        actual.recurring must be(eventType.recurring)
      }
    }

    it("replace a specimen spec") {
      val specimenDefinition  = factory.createCollectedSpecimenDefinition
      val specimenDefinition2 = factory.createCollectedSpecimenDefinition.copy(id = specimenDefinition.id)
      val eventType           = factory.createCollectionEventType.copy(specimenDefinitions = Set(specimenDefinition))

      eventType.withSpecimenDefinition(specimenDefinition2) mustSucceed { actual =>
        actual.studyId must be(eventType.studyId)
        actual.id must be(eventType.id)
        actual.version must be(eventType.version + 1)
        actual.name must be(eventType.name)
        actual.description must be(eventType.description)
        actual.recurring must be(eventType.recurring)
      }
    }

    it("remove a specimen spec") {
      val specimenDefinition = factory.createCollectedSpecimenDefinition
      val eventType          = factory.createCollectionEventType.copy(specimenDefinitions = Set(specimenDefinition))

      eventType.removeSpecimenDefinition(specimenDefinition.id) mustSucceed { actual =>
        actual.studyId must be(eventType.studyId)
        actual.id must be(eventType.id)
        actual.version must be(eventType.version + 1)
        actual.name must be(eventType.name)
        actual.description must be(eventType.description)
        actual.recurring must be(eventType.recurring)
      }
    }

    it("not allow adding a specimen spec with a duplicate name") {
      val specimenDefinition = factory.createCollectedSpecimenDefinition
      val specimenDefinition2 =
        factory.createCollectedSpecimenDefinition.copy(name = specimenDefinition.name)
      val eventType = factory.createCollectionEventType.copy(specimenDefinitions = Set(specimenDefinition))

      eventType
        .withSpecimenDefinition(specimenDefinition2)
        .mustFail("EntityCriteriaError: specimen definition name already used.*")
    }
  }

  override def createEntity(): CollectionEventType =
    factory.createCollectionEventType.copy(annotationTypes = Set.empty)

  override def getAnnotationTypeSet(entity: CollectionEventType): Set[AnnotationType] =
    entity.annotationTypes

  override def addAnnotationType(
      entity:         CollectionEventType,
      annotationType: AnnotationType
    ): DomainValidation[CollectionEventType] =
    entity.withAnnotationType(annotationType)

  override def removeAnnotationType(
      entity: CollectionEventType,
      id:     AnnotationTypeId
    ): DomainValidation[CollectionEventType] =
    entity.removeAnnotationType(id)

  describe("A collection event type's annotation type set") {

    annotationTypeSetSharedBehaviour

  }

}
