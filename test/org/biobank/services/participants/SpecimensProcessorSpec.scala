package org.biobank.services.participants

import akka.actor._
import akka.pattern._
import javax.inject.{Inject, Named}
import org.biobank.fixtures._
import org.biobank.domain.studies.{CollectionEventTypeRepository, StudyRepository}
import org.biobank.domain.centres.CentreRepository
import org.biobank.domain.participants._
import org.biobank.domain.processing._
import org.biobank.services._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import play.api.libs.json._
import scala.language.reflectiveCalls
import scala.concurrent.duration._

case class NamedSpecimensProcessor @Inject()(@Named("specimensProcessor") processor: ActorRef)

class SpecimensProcessorSpec
    extends ProcessorTestFixture with SpecimenSpecFixtures with PresistenceQueryEvents {

  import org.biobank.TestUtils._
  import org.scalatest.matchers.must.Matchers._
  import org.biobank.infrastructure.commands.SpecimenCommands._
  import org.biobank.infrastructure.events.SpecimenEvents._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private var specimensProcessor = app.injector.instanceOf[NamedSpecimensProcessor].processor

  private val studyRepository = app.injector.instanceOf[StudyRepository]

  private val centreRepository = app.injector.instanceOf[CentreRepository]

  private val collectionEventTypeRepository = app.injector.instanceOf[CollectionEventTypeRepository]

  private val participantRepository = app.injector.instanceOf[ParticipantRepository]

  private val collectionEventRepository = app.injector.instanceOf[CollectionEventRepository]

  private val specimenRepository = app.injector.instanceOf[SpecimenRepository]

  private val nameGenerator = new NameGenerator(this.getClass)

  override def beforeEach() = {
    specimenRepository.removeAll
    super.beforeEach()
  }

  private def restartProcessor(processor: ActorRef) = {
    gracefulStop(processor, 5 seconds, PoisonPill).map { _ =>
      val actor = system.actorOf(
        Props(
          new SpecimensProcessor(specimenRepository,
                                 collectionEventRepository,
                                 collectionEventTypeRepository,
                                 app.injector.instanceOf[CeventSpecimenRepository],
                                 app.injector.instanceOf[ProcessingEventInputSpecimenRepository],
                                 app.injector.instanceOf[SnapshotWriter])
        ),
        "specimens-processor-id-2"
      )
      Thread.sleep(250)
      actor
    }
  }

  describe("A specimens processor must") {

    it("allow recovery from journal", PersistenceTest) {
      val f = createEntitiesAndSpecimens

      centreRepository.put(f.centre)
      studyRepository.put(f.study)
      collectionEventTypeRepository.put(f.ceventType)
      participantRepository.put(f.participant)
      collectionEventRepository.put(f.cevent)

      f.specimens.foreach { specimen =>
        val specimenInfo = SpecimenInfo(inventoryId = specimen.inventoryId,
                                        specimenDefinitionId = specimen.specimenDefinitionId.id,
                                        timeCreated          = specimen.timeCreated,
                                        locationId           = specimen.originLocationId.id,
                                        amount               = specimen.amount)

        val cmd = AddSpecimensCmd(sessionUserId = nameGenerator.next[String],
                                  collectionEventId = f.cevent.id.id,
                                  specimenData      = List(specimenInfo))

        val v = (specimensProcessor ? cmd).mapTo[ServiceValidation[SpecimenEvent]].futureValue
        v.isSuccess must be(true)
        specimenRepository.getValues.map { s =>
          s.inventoryId
        } must contain(specimen.inventoryId)
      }

      specimenRepository.removeAll
      specimensProcessor = restartProcessor(specimensProcessor).futureValue

      specimenRepository.getValues.size must be(f.specimens.size)
      f.specimens.foreach { specimen =>
        specimenRepository.getValues.map { s =>
          s.inventoryId
        } must contain(specimen.inventoryId)
      }
    }

    it("recovers a snapshot", PersistenceTest) {
      val cevent           = factory.createCollectionEvent
      val snapshotFilename = "testfilename"
      val specimens = (1 to 2).map { _ =>
        factory.createUsableSpecimen
      }
      val ceventSpecimens = specimens.map { specimen =>
        CeventSpecimen(cevent.id, specimen.id)
      }
      val snapshotSpecimen = specimens(0)
      val snapshotState    = SpecimensProcessor.SnapshotState(Set(snapshotSpecimen), Set(ceventSpecimens(0)))

      Mockito.when(snapshotWriterMock.save(anyString, anyString)).thenReturn(snapshotFilename);
      Mockito
        .when(snapshotWriterMock.load(snapshotFilename))
        .thenReturn(Json.toJson(snapshotState).toString);

      specimens.foreach(specimenRepository.put)
      (specimensProcessor ? "snap").mapTo[String].futureValue

      specimenRepository.removeAll
      specimensProcessor = restartProcessor(specimensProcessor).futureValue

      specimenRepository.getValues.size must be(1)
      specimenRepository.getByKey(snapshotSpecimen.id) mustSucceed { repoSpecimen =>
        repoSpecimen.inventoryId must be(snapshotSpecimen.inventoryId)
        ()
      }
    }

  }

}
