package org.biobank.controllers.participants

import java.time.format.DateTimeFormatter
import org.biobank.TestUtils
import java.time.OffsetDateTime
import org.biobank.controllers._
import org.biobank.domain._
import org.biobank.domain.participants._
import org.biobank.domain.study._
import org.scalatest.prop.TableDrivenPropertyChecks._
import play.api.libs.json._
import play.api.test.Helpers._
import scalaz.Validation.FlatMap._

/**
 * Tests the REST API for [[CollectionEvents]].
 */
class CollectionEventsControllerSpec extends StudyAnnotationsControllerSharedSpec[CollectionEvent] {

  import org.biobank.TestUtils._
  import org.biobank.AnnotationTestUtils._

  def uri(): String = "/api/participants/cevents"

  def uri(collectionEvent: CollectionEvent): String =
    uri + s"/${collectionEvent.id}"

  def uri(participantId: ParticipantId): String =
    uri + s"/${participantId.id}"

  def uri(participantId: ParticipantId, cevent: CollectionEvent): String =
    uri(participantId) + s"/${cevent.id.id}"

  def uri(participantId: ParticipantId, cevent: CollectionEvent, version: Long): String =
    uri(participantId) + s"/${cevent.id.id}/$version"

  def uri(participant: Participant): String =
    uri(participant.id)

  def uri(participant: Participant, cevent: CollectionEvent): String =
    uri(participant.id, cevent)

  def uri(participant: Participant, cevent: CollectionEvent, version: Long): String =
    uri(participant.id, cevent, version)

  def uriWithQuery(participant: Participant, cevent: CollectionEvent): String =
    uri(participant.id) + s"?ceventId=${cevent.id.id}"

  def uriWithVisitNumber(participant: Participant, cevent: CollectionEvent): String =
    uri + s"/visitNumber/${participant.id.id}/${cevent.visitNumber}"

  def listUri(participantId: ParticipantId): String =
    uri + s"/list/${participantId.id}"

  def updateUri(cevent: CollectionEvent, path: String): String =
    uri + s"/$path/${cevent.id.id}"

  protected def createEntity(annotationTypes: Set[AnnotationType],
                             annotations:     Set[Annotation]): CollectionEvent = {
    val (_, _, ceventType) = createEntities

    collectionEventTypeRepository.put(ceventType.copy(annotationTypes = annotationTypes))
    val cevent = factory.createCollectionEvent.copy(annotations = annotations)
    collectionEventRepository.put(cevent)
    cevent
  }

  protected def entityFromRepository(id: String): DomainValidation[CollectionEvent] = {
    collectionEventRepository.getByKey(CollectionEventId(id))
  }

  protected def entityName(): String = "collection event"

  protected def updateUri(cevent: CollectionEvent): String = updateUri(cevent, "annot")

  protected def getStudy(cevent: CollectionEvent): DomainValidation[EnabledStudy] = {
    for {
      participant <- participantRepository.getByKey(cevent.participantId)
      study       <- studyRepository.getEnabled(participant.studyId)
    } yield study
  }

  /** Converts a collectionEvent into an Add command.
   */
  def collectionEventToAddJson(collectionEvent: CollectionEvent,
                               annotations: List[Annotation] = List.empty) = {
    Json.obj(
      "collectionEventTypeId" -> collectionEvent.collectionEventTypeId,
      "timeCompleted"         -> collectionEvent.timeCompleted,
      "visitNumber"           -> collectionEvent.visitNumber,
      "annotations"           -> annotations.map(annotationToJson(_))
    )
  }

  def compareObjs(jsonList: List[JsObject], cevents: List[CollectionEvent]) = {
    val ceventsMap = cevents.map { cevent => (cevent.id, cevent) }.toMap
    jsonList.foreach { jsonObj =>
      val jsonId = CollectionEventId((jsonObj \ "id").as[String])
      compareObj(jsonObj, ceventsMap(jsonId))
    }
  }

  /**
   * create pairs of annotation types and annotation of each value type plus a second of type select that
   * allows multiple selections
   *
   * the result is a map where the keys are the annotation types and the values are the corresponding
   * annotations
   */
  def createAnnotationsAndTypes() = {
    val options = Seq(nameGenerator.next[String],
                      nameGenerator.next[String],
                      nameGenerator.next[String])

    (AnnotationValueType.values.map { vt =>
       vt match {
         case AnnotationValueType.Select   =>
           (factory.createAnnotationType(vt, Some(1), options),
            factory.createAnnotation)
         case _ =>
           (factory.createAnnotationType(vt, None, Seq.empty),
            factory.createAnnotation)
       }
     }.toList ++ List(
       (factory.createAnnotationType(AnnotationValueType.Select, Some(2), options),
        factory.createAnnotation))).toMap
  }

  def createEntities(): (EnabledStudy, Participant, CollectionEventType) = {
    val study = factory.createEnabledStudy
    studyRepository.put(study)

    val ceventType = factory.createCollectionEventType.copy(studyId = study.id,
                                                            annotationTypes = Set.empty)
    collectionEventTypeRepository.put(ceventType)

    val participant = factory.createParticipant.copy(studyId = study.id)
    participantRepository.put(participant)

    (study, participant, ceventType)
  }

  def createEntities(fn: (EnabledStudy, Participant, CollectionEventType) => Unit): Unit = {
    val (study, participant, ceventType) = createEntities
    fn(study, participant, ceventType)
    ()
  }

  def addOnNonEnabledStudy(study: Study, cevent: CollectionEvent) = {
    study must not be an [EnabledStudy]

    studyRepository.put(study)

    val cmdJson = collectionEventToAddJson(cevent);
    val json = makeRequest(POST, uri(cevent.participantId), BAD_REQUEST, cmdJson)

    (json \ "status").as[String] must include ("error")

    (json \ "message").as[String] must include regex ("InvalidStatus: study not enabled")
  }

  def updateOnNonEnabledStudy(study:       Study,
                              participant: Participant,
                              ceventType:  CollectionEventType,
                              path:        String,
                              jsonField:   JsObject) {
    study must not be an [EnabledStudy]

    studyRepository.put(study)

    val cevent = factory.createCollectionEvent.copy(participantId = participant.id,
                                                    collectionEventTypeId = ceventType.id)
    collectionEventRepository.put(cevent)

    val reqJson = jsonField ++ Json.obj("expectedVersion" -> cevent.version)

    val json = makeRequest(POST, updateUri(cevent, path), BAD_REQUEST, reqJson)

    (json \ "status").as[String] must include ("error")

    (json \ "message").as[String] must include regex("InvalidStatus: study not enabled")

    ()
  }

  def updateWithInvalidVersion(participant: Participant,
                               ceventType:  CollectionEventType,
                               path:        String,
                               jsonField:   JsObject) {
    val cevent = factory.createCollectionEvent.copy(participantId = participant.id,
                                                    collectionEventTypeId = ceventType.id)
    collectionEventRepository.put(cevent)

    val reqJson = jsonField ++ Json.obj("expectedVersion" -> (cevent.version + 1))

    val json = makeRequest(POST, updateUri(cevent, path), BAD_REQUEST, reqJson)

    (json \ "status").as[String] must include ("error")

    (json \ "message").as[String] must include regex (".*expected version doesn't match current version.*")

    ()
  }

  def updateOnInvalidCevent(participant: Participant,
                            ceventType:  CollectionEventType,
                            path:        String,
                            jsonField:   JsObject) {
    val cevent = factory.createCollectionEvent.copy(participantId = participant.id,
                                                    collectionEventTypeId = ceventType.id)

    val reqJson = jsonField ++ Json.obj("expectedVersion" -> cevent.version)

    val json = makeRequest(POST, updateUri(cevent, path), NOT_FOUND, reqJson)

    (json \ "status").as[String] must include ("error")

    (json \ "message").as[String] must include regex ("IdNotFound.*collection event")

    ()
  }

  def removeOnNonEnabledStudy(study: Study, cevent: CollectionEvent) = {
    study must not be an [EnabledStudy]

    studyRepository.put(study)
    collectionEventRepository.put(cevent)

    val json = makeRequest(DELETE, uri(cevent.participantId, cevent, cevent.version), BAD_REQUEST)

    (json \ "status").as[String] must include ("error")

    (json \ "message").as[String] must include regex("InvalidStatus: study not enabled")
  }

  describe("Collection Event REST API") {

    describe("GET /api/participants/cevents/:ceventId") {

      it("get a single collection event for a participant") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (0 until 2).map { x =>
              val cevent = factory.createCollectionEvent
              collectionEventRepository.put(cevent)
              cevent
            }

          val ceventToGet = cevents(0)

          val json = makeRequest(GET, uri(ceventToGet))

          (json \ "status").as[String] must include ("success")

          val jsonObj = (json \ "data").as[JsObject]
          compareObj(jsonObj, ceventToGet)
          ()
        }
      }

      it("fail when querying for a single collection event and ID is invalid") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent

          val json = makeRequest(GET, uri(cevent), NOT_FOUND)

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("IdNotFound: collection event id")

          ()
        }
      }

    }

    describe("GET /api/participants/cevents/list/{participantId}") {

      it("list none") {
        createEntities { (study, participant, ceventType) =>
          PagedResultsSpec(this).emptyResults(listUri(participant.id))
        }
      }

      it("list a single collection event") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent
          collectionEventRepository.put(cevent)

          val jsonItems = PagedResultsSpec(this).multipleItemsResult(
              uri = listUri(participant.id),
              offset = 0,
              total = 1,
              maybeNext = None,
              maybePrev = None)
          jsonItems must have size 1
          //log.info(s"--> $jsonItems")
          compareObjs(jsonItems, List(cevent))
        }
      }

      it("get all collection events for a participant") {
        createEntities { (study, participant, ceventType) =>
          val participants = (0 until 2).map { x =>
              val participant = factory.createParticipant.copy(studyId = study.id)
              participantRepository.put(participant)
              participant
            }

          val cevents = participants.map { participant =>
              (participant.id -> (0 until 2).map { x =>
                 val cevent = factory.createCollectionEvent.copy(participantId = participant.id)
                 collectionEventRepository.put(cevent)
                 cevent
               })
            }.toMap

          participants.foreach { participant =>
            val participantCevents = cevents(participant.id).toList

            val jsonItems = PagedResultsSpec(this).multipleItemsResult(
                uri       = listUri(participant.id),
                offset    = 0,
                total     = cevents.size.toLong,
                maybeNext = None,
                maybePrev = None)
            jsonItems must have size cevents.size.toLong
            //log.info(s"--> $jsonItems")
            compareObjs(jsonItems, participantCevents)
          }
        }
      }

      it("list collection events sorted by visit number") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (1 to 4).map { visitNumber =>
              val cevent = factory.createCollectionEvent.copy(
                  participantId = participant.id,
                  visitNumber = visitNumber)
              collectionEventRepository.put(cevent)
              cevent
            }

          val sortExprs = Table("sort expressions", "visitNumber", "-visitNumber")
          forAll(sortExprs) { sortExpr =>
            List("asc", "desc").foreach{ ordering =>
              val jsonItems = PagedResultsSpec(this).multipleItemsResult(
                  uri         = listUri(participant.id),
                  queryParams = Map("sort" -> sortExpr),
                  offset      = 0,
                  total       = cevents.size.toLong,
                  maybeNext   = None,
                  maybePrev   = None)

              jsonItems must have size cevents.size.toLong
              if (sortExpr == sortExprs(0)) {
                compareObj(jsonItems(0), cevents(0))
                compareObj(jsonItems(1), cevents(1))
                compareObj(jsonItems(2), cevents(2))
                compareObj(jsonItems(3), cevents(3))
              } else {
                compareObj(jsonItems(0), cevents(3))
                compareObj(jsonItems(1), cevents(2))
                compareObj(jsonItems(2), cevents(1))
                compareObj(jsonItems(3), cevents(0))
              }
            }

          }
        }
      }

      it("list collection events sorted by time completed") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (1 to 4).map { hour =>
              val cevent = factory.createCollectionEvent.copy(
                  participantId = participant.id,
                  timeCompleted = OffsetDateTime.now.withHour(hour))
              collectionEventRepository.put(cevent)
              cevent
            }

          val sortExprs = Table("sort expressions", "timeCompleted", "-timeCompleted")
          forAll(sortExprs) { sortExpr =>
            val jsonItems = PagedResultsSpec(this).multipleItemsResult(
                uri         = listUri(participant.id),
                queryParams = Map("sort" -> sortExpr),
                offset      = 0,
                total       = cevents.size.toLong,
                maybeNext   = None,
                maybePrev   = None)

            jsonItems must have size cevents.size.toLong
            if (sortExpr == sortExprs(0)) {
              compareObj(jsonItems(0), cevents(0))
              compareObj(jsonItems(1), cevents(1))
              compareObj(jsonItems(2), cevents(2))
              compareObj(jsonItems(3), cevents(3))
            } else {
              compareObj(jsonItems(0), cevents(3))
              compareObj(jsonItems(1), cevents(2))
              compareObj(jsonItems(2), cevents(1))
              compareObj(jsonItems(3), cevents(0))
            }
          }
          ()
        }
      }

      it("list the first collection event in a paged query") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (1 to 4).map { hour =>
              val cevent = factory.createCollectionEvent.copy(
                  participantId = participant.id,
                  timeCompleted = OffsetDateTime.now.withHour(hour))
              collectionEventRepository.put(cevent)
              cevent
            }

          val jsonItem = PagedResultsSpec(this).singleItemResult(
              uri         = listUri(participant.id),
              queryParams = Map("sort" -> "timeCompleted", "limit" -> "1"),
              total       = cevents.size.toLong,
              maybeNext   = Some(2))

          compareObj(jsonItem, cevents(0))
          ()
        }
      }

      it("list the last collection event in a paged query") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (1 to 4).map { hour =>
              val cevent = factory.createCollectionEvent.copy(
                  participantId = participant.id,
                  timeCompleted = OffsetDateTime.now.withHour(hour))
              collectionEventRepository.put(cevent)
              cevent
            }

          val jsonItem = PagedResultsSpec(this).singleItemResult(
              uri         = listUri(participant.id),
              queryParams = Map("sort" -> "timeCompleted", "page" -> "4", "limit" -> "1"),
              total       = cevents.size.toLong,
              offset      = 3,
              maybeNext   = None,
              maybePrev   = Some(3))

          compareObj(jsonItem, cevents(3))
          ()
        }
      }

      it("fail when using an invalid query parameters") {
        createEntities { (study, participant, ceventType) =>
          val uri = listUri(participant.id)

          PagedResultsSpec(this).failWithNegativePageNumber(uri)
          PagedResultsSpec(this).failWithInvalidPageNumber(uri)
          PagedResultsSpec(this).failWithNegativePageSize(uri)
          PagedResultsSpec(this).failWithInvalidPageSize(uri, 100);
          PagedResultsSpec(this).failWithInvalidSort(uri)
          ()
        }
      }

      it("fail for invalid participant id") {
        val participant = factory.createParticipant
        val json = makeRequest(GET, listUri(participant.id), NOT_FOUND)

        (json \ "status").as[String] must include ("error")

        (json \ "message").as[String] must include regex ("IdNotFound.*participant id")
      }

    }

    describe("GET /api/participants/cevents/visitNumber/:participantId/:vn") {

      it("get a collection event by visit number") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (1 to 2).map { visitNumber =>
              val cevent = factory.createCollectionEvent.copy(visitNumber = visitNumber)
              collectionEventRepository.put(cevent)
              cevent
            }

          val ceventToGet = cevents(0)
          val json = makeRequest(GET, uriWithVisitNumber(participant, ceventToGet))

          (json \ "status").as[String] must include ("success")
          val jsonObj = (json \ "data").as[JsObject]
          compareObj(jsonObj, ceventToGet)
          ()
        }
      }

      it("fail for invalid participant id when querying for a collection event with a visit number") {
        val participant = factory.createParticipant
        val cevent = factory.createCollectionEvent

        studyRepository.put(factory.defaultEnabledStudy)

        val json = makeRequest(GET, uriWithVisitNumber(participant, cevent), NOT_FOUND)

        (json \ "status").as[String] must include ("error")

        (json \ "message").as[String] must include regex ("NotFound.*collection event")
      }

      it("fail when querying for a collection event with a visit number") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent

          val json = makeRequest(GET, uriWithVisitNumber(participant, cevent), NOT_FOUND)

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("collection event does not exist")

          ()
        }
      }
    }

    describe("POST /api/participants/cevents/:participantId") {

      it("add a collection event with no annotations") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent
          cevent.annotations must have size 0

          val json = makeRequest(POST, uri(participant), collectionEventToAddJson(cevent))

          (json \ "status").as[String] must include ("success")

          val id = (json \ "data" \ "id").as[String]

          collectionEventRepository.getByKey(CollectionEventId(id)) mustSucceed { repoCe =>
            compareObj((json \ "data").as[JsObject], repoCe)

            repoCe must have (
              'participantId          (cevent.participantId),
              'collectionEventTypeId  (cevent.collectionEventTypeId),
              'version                (0),
              'visitNumber            (cevent.visitNumber)
            )

            repoCe.annotations must have size 0

            TestUtils.checkTimeStamps(repoCe.timeCompleted, cevent.timeCompleted, TimeCoparisonSeconds)
            checkTimeStamps(repoCe, OffsetDateTime.now, None)
          }
        }
      }

      it("fail when adding and visit number is already used") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent

          collectionEventRepository.put(cevent)

          val json = makeRequest(POST,
                                 uri(participant),
                                 FORBIDDEN,
                                 collectionEventToAddJson(cevent))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("a collection event with this visit number already exists")

          ()
        }
      }

      it("add a collection event with annotations") {
        createEntities { (study, participant, ceventType) =>
          val annotTypes = createAnnotationsAndTypes
          val annotations = annotTypes.values.toList

          collectionEventTypeRepository.put(
            ceventType.copy(annotationTypes = annotTypes.keys.toSet))

          val cevent = factory.createCollectionEvent
          val json = makeRequest(POST,
                                 uri(participant),
                                 collectionEventToAddJson(cevent, annotations))

          (json \ "status").as[String] must include ("success")

          (json \ "data" \ "annotations").as[List[JsObject]] must have size annotTypes.size.toLong
          val jsonAnnotations = (json \ "data" \ "annotations").as[List[JsObject]]
          jsonAnnotations must have size annotations.size.toLong

          jsonAnnotations.foreach { jsonAnnotation =>
            val jsonAnnotationTypeId = (jsonAnnotation \ "annotationTypeId").as[String]
            val annotation = annotations.find(x => x.annotationTypeId.id == jsonAnnotationTypeId).value
            compareAnnotation(jsonAnnotation, annotation)
          }
        }
      }

      it("be able to add a collection event with an empty, non required, number annotation") {
        createEntities { (study, participant, ceventType) =>
          val annotType = factory
            .createAnnotationType(AnnotationValueType.Number, None, Seq.empty)
            .copy(required = false)
          val annotation = factory.createAnnotation.copy(numberValue = Some(""))

          collectionEventTypeRepository.put(ceventType.copy(annotationTypes = Set(annotType)))

          val cevent = factory.createCollectionEvent
          val json = makeRequest(POST,
                                 uri(participant),
                                 collectionEventToAddJson(cevent, List(annotation)))

          (json \ "status").as[String] must include ("success")

          (json \ "data" \ "annotations").as[List[JsObject]] must have size 1
          val jsonAnnotations = (json \ "data" \ "annotations").as[List[JsObject]]
          jsonAnnotations must have size 1

          jsonAnnotations.foreach { jsonAnnotation =>
            compareAnnotation(jsonAnnotation, annotation)
          }
        }
      }

      it("fail when adding and participant and collection event type not in same study") {
        createEntities { (study, participant, ceventType) =>
          val otherStudy = factory.createDisabledStudy
          val otherCeventType = factory.createCollectionEventType.copy(studyId = otherStudy.id)

          studyRepository.put(otherStudy)
          collectionEventTypeRepository.put(otherCeventType)

          val cevent = factory.createCollectionEvent
          val json = makeRequest(method         = POST,
                                 path           = uri(participant),
                                 expectedStatus = BAD_REQUEST,
                                 json           = collectionEventToAddJson(cevent))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include (
            "participant and collection event type not in the same study")

          ()
        }
      }

      it("fail when adding collection event with duplicate visit number") {
        createEntities { (study, participant, ceventType) =>
          val cevent1 = factory.createCollectionEvent
          collectionEventRepository.put(cevent1)

          val cevent2 = factory.createCollectionEvent.copy(visitNumber = cevent1.visitNumber)
          val json = makeRequest(method         = POST,
                                 path           = uri(participant),
                                 expectedStatus = FORBIDDEN,
                                 json           = collectionEventToAddJson(cevent2))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("a collection event with this visit number already exists")

          ()
        }
      }

      it("fail when missing a required annotation type") {
        createEntities { (study, participant, ceventType) =>
          val annotType = factory.createAnnotationType.copy(required = true)

          collectionEventTypeRepository.put(
            ceventType.copy(annotationTypes = Set(annotType)))

          val cevent = factory.createCollectionEvent.copy(annotations = Set.empty)
          val json = makeRequest(method         = POST,
                                 path           = uri(participant),
                                 expectedStatus = BAD_REQUEST,
                                 json           = collectionEventToAddJson(cevent))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("missing required annotation type(s)")

          ()
        }
      }

      it("fail when using annotations and collection event type has no annotations") {
        createEntities { (study, participant, ceventType) =>
          val annotation = factory.createAnnotation
            .copy(annotationTypeId = AnnotationTypeId(nameGenerator.next[Annotation]))

          val cevent = factory.createCollectionEvent
          val json = makeRequest(method         = POST,
                                 path           = uri(participant),
                                 expectedStatus = BAD_REQUEST,
                                 json           = collectionEventToAddJson(cevent, List(annotation)))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("no annotation types")

          ()
        }
      }

      it("fail for an annotation with an invalid annotation type id") {
        createEntities { (study, participant, ceventType) =>
          val annotType = factory.createAnnotationType

          // update the collection event type with annotation type data
          collectionEventTypeRepository.put(ceventType.copy(annotationTypes = Set(annotType)))

          val annotation = factory.createAnnotation.copy(
              annotationTypeId = AnnotationTypeId(nameGenerator.next[Annotation]))

          val cevent = factory.createCollectionEvent
          val json = makeRequest(POST,
                                 uri(participant),
                                 BAD_REQUEST,
                                 collectionEventToAddJson(cevent, List(annotation)))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include (
            "annotation(s) do not belong to annotation types")

          ()
        }
      }

      it("fail for more than one annotation with the same annotation type ID") {
        createEntities { (study, participant, ceventType) =>
          val annotType = factory.createAnnotationType

          // update the collection event type with annotation type data
          collectionEventTypeRepository.put(ceventType.copy(annotationTypes = Set(annotType)))

          val annotation = factory.createAnnotationWithValues(annotType)
          annotation.stringValue mustBe defined

          val cevent = factory.createCollectionEvent
          val annotations = List(annotation,
                                 annotation.copy(stringValue = Some(nameGenerator.next[Annotation])))

          val json = makeRequest(method         = POST,
                                 path           = uri(participant),
                                 expectedStatus = BAD_REQUEST,
                                 json           = collectionEventToAddJson(cevent, annotations))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("duplicate annotations")

          ()
        }
      }

      it("not add a collection event on an disabled study") {
        createEntities { (study, participant, ceventType) =>
          val disabledStudy = factory.createDisabledStudy.copy(id = study.id)
          val cevent = factory.createCollectionEvent
          addOnNonEnabledStudy(disabledStudy, cevent)
          ()
        }
      }

      it("not add a collection event on an retired study") {
        createEntities { (study, participant, ceventType) =>
          val retiredStudy = factory.createRetiredStudy.copy(id = study.id)
          val cevent = factory.createCollectionEvent
          addOnNonEnabledStudy(retiredStudy, cevent)
          ()
        }
      }

    }

    describe("POST /api/participants/cevents/visitNumber/:ceventId") {

      it("update the visit number on a collection event") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent
          val newVisitNumber = cevent.visitNumber + 1

          collectionEventRepository.put(cevent)
          cevent.annotations must have size 0

          val json = makeRequest(POST,
                                 updateUri(cevent, "visitNumber"),
                                 Json.obj("expectedVersion" -> Some(cevent.version),
                                          "visitNumber"     -> newVisitNumber))

          (json \ "status").as[String] must include ("success")

          collectionEventRepository.getByKey(cevent.id) mustSucceed { repoCe =>
            repoCe must have (
              'id                     (cevent.id),
              'participantId          (cevent.participantId),
              'collectionEventTypeId  (cevent.collectionEventTypeId),
              'version                (cevent.version + 1),
              'visitNumber            (newVisitNumber),
              'annotations            (cevent.annotations)
            )

            TestUtils.checkTimeStamps(repoCe.timeCompleted, cevent.timeCompleted, TimeCoparisonSeconds)
            checkTimeStamps(repoCe, cevent.timeAdded, OffsetDateTime.now)
          }
        }
      }

      it("fail when updating visit number to one already used") {
        createEntities { (study, participant, ceventType) =>
          val cevents = (1 to 2).map { visitNumber =>
              val cevent = factory.createCollectionEvent.copy(visitNumber = visitNumber)
              collectionEventRepository.put(cevent)
              cevent
            }

          val ceventToUpdate = cevents(0)
          val duplicateVisitNumber = cevents(1).visitNumber

          val json = makeRequest(POST,
                                 updateUri(ceventToUpdate, "visitNumber"),
                                 FORBIDDEN,
                                 Json.obj("expectedVersion" -> Some(ceventToUpdate.version),
                                          "visitNumber"     -> duplicateVisitNumber))

          (json \ "status").as[String] must include ("error")

          (json \ "message").as[String] must include ("a collection event with this visit number already exists")

          ()
        }
      }

      it("not update a collection event's visit number on a non enabled study") {
        createEntities { (study, participant, ceventType) =>
          val newTimeCompleted = 2

          study.disable mustSucceed { disabledStudy =>
            updateOnNonEnabledStudy(disabledStudy,
                                    participant,
                                    ceventType,
                                    "visitNumber",
                                    Json.obj("visitNumber" -> newTimeCompleted))

            disabledStudy.retire mustSucceed { retiredStudy =>
              updateOnNonEnabledStudy(retiredStudy,
                                      participant,
                                      ceventType,
                                      "visitNumber",
                                      Json.obj("visitNumber" -> newTimeCompleted))
            }
          }
        }
      }

      it("fail when updating visit number and collection event ID is invalid") {
        createEntities { (study, participant, ceventType) =>
          updateOnInvalidCevent(participant,
                                ceventType,
                                "visitNumber",
                                Json.obj("visitNumber" -> 1))
        }
      }

      it("fail when updating visit number with an invalid version") {
        createEntities { (study, participant, ceventType) =>
          updateWithInvalidVersion(participant,
                                   ceventType,
                                   "visitNumber",
                                   Json.obj("visitNumber" -> 1))
        }
      }

    }

    describe("POST /api/participants/cevents/timeCompleted/:ceventId") {

      it("update the time completed on a collection event") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent
          val newTimeCompleted = cevent.timeCompleted.minusMonths(2)

          collectionEventRepository.put(cevent)
          cevent.annotations must have size 0

          val json = makeRequest(POST,
                                 updateUri(cevent, "timeCompleted"),
                                 Json.obj("expectedVersion" -> Some(cevent.version),
                                          "timeCompleted"     -> newTimeCompleted))

          (json \ "status").as[String] must include ("success")

          collectionEventRepository.getByKey(cevent.id) mustSucceed { repoCe =>
            repoCe must have (
              'id                     (cevent.id),
              'participantId          (cevent.participantId),
              'collectionEventTypeId  (cevent.collectionEventTypeId),
              'version                (cevent.version + 1),
              'visitNumber            (cevent.visitNumber),
              'annotations            (cevent.annotations)
            )

            TestUtils.checkTimeStamps(repoCe.timeCompleted, newTimeCompleted, TimeCoparisonSeconds)
            checkTimeStamps(repoCe, cevent.timeAdded, OffsetDateTime.now)
          }
        }
      }

      it("not update a collection event's time completed on a non enabled study") {
        createEntities { (study, participant, ceventType) =>
          val newTimeCompleted = OffsetDateTime.now.minusMonths(2)

          study.disable mustSucceed { disabledStudy =>
            updateOnNonEnabledStudy(disabledStudy,
                                    participant,
                                    ceventType,
                                    "timeCompleted",
                                    Json.obj("timeCompleted" -> newTimeCompleted))

            disabledStudy.retire mustSucceed { retiredStudy =>
              updateOnNonEnabledStudy(retiredStudy,
                                      participant,
                                      ceventType,
                                      "timeCompleted",
                                      Json.obj("timeCompleted" -> newTimeCompleted))
            }
          }
        }
      }

      it("fail when updating time completed and collection event ID is invalid") {
        createEntities { (study, participant, ceventType) =>
          val timeCompleted = OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          updateOnInvalidCevent(participant,
                                ceventType,
                                "timeCompleted",
                                Json.obj("timeCompleted" -> timeCompleted))
        }
      }

      it("fail when updating time completed with an invalid version") {
        createEntities { (study, participant, ceventType) =>
          val timeCompleted = OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          updateWithInvalidVersion(participant,
                                   ceventType,
                                   "timeCompleted",
                                   Json.obj("timeCompleted" -> timeCompleted))
        }
      }

    }

    describe("POST /api/participants/cevents/annot/:ceventId") {

      annotationTypeUpdateSharedBehaviour

      it("fail when adding annotation and collection event ID is invalid") {
        createEntities { (study, participant, ceventType) =>

          val annotationType = factory.createAnnotationType
          val annotation = factory.createAnnotation

          collectionEventTypeRepository.put(ceventType.copy(annotationTypes = Set(annotationType)))

          updateOnInvalidCevent(participant, ceventType, "annot", annotationToJson(annotation))
        }
      }

    }

    describe("DELETE /api/participants/cevents/annot/:ceventId/:annotTypeId/:ver") {

      annotationTypeRemoveSharedBehaviour

    }

    describe("DELETE /api/participants/cevents/:participantId/:ceventId/:ver") {

      it("remove a collection event") {
        createEntities { (study, participant, ceventType) =>
          val cevent = factory.createCollectionEvent
          collectionEventRepository.put(cevent)

          val json = makeRequest(DELETE, uri(participant, cevent, cevent.version))

          (json \ "status").as[String] must include ("success")

          (json \ "data").as[Boolean] must be (true)

          collectionEventRepository.getByKey(cevent.id) mustFail("IdNotFound: collection event.*")
        }
      }

      it("not remove a collection event from an disabled study") {
        createEntities { (study, participant, ceventType) =>
          val disabledStudy = factory.createDisabledStudy.copy(id = study.id)
          val cevent = factory.createCollectionEvent
          removeOnNonEnabledStudy(disabledStudy, cevent)
          ()
        }
      }

      it("not remove a collection event from an retired study") {
        createEntities { (study, participant, ceventType) =>
          val retiredStudy = factory.createRetiredStudy.copy(id = study.id)
          val cevent = factory.createCollectionEvent
          removeOnNonEnabledStudy(retiredStudy, cevent)
          ()
        }
      }

    }

  }
}
