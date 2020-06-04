package org.biobank.services.studies

import akka.actor._
import akka.pattern._
import javax.inject.{Inject, Named}
import org.biobank.fixtures._
import org.biobank.domain.studies._
import org.biobank.services._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import play.api.libs.json._
import scala.concurrent.duration._

final case class NamedStudiesProcessor @Inject()(@Named("studiesProcessor") processor: ActorRef)

class StudiesProcessorSpec extends ProcessorTestFixture {

  import org.biobank.TestUtils._
  import org.scalatest.matchers.must.Matchers._
  import org.biobank.infrastructure.commands.StudyCommands._
  import org.biobank.infrastructure.events.StudyEvents._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private var studiesProcessor = app.injector.instanceOf[NamedStudiesProcessor].processor

  private val studyRepository = app.injector.instanceOf[StudyRepository]

  override def beforeEach() = {
    studyRepository.removeAll
    super.beforeEach()
  }

  private def restartProcessor(processor: ActorRef) = {
    gracefulStop(processor, 5 seconds, PoisonPill).map { _ =>
      val actor =
        system.actorOf(Props(
                         new StudiesProcessor(studyRepository,
                                              app.injector.instanceOf[CollectionEventTypeRepository],
                                              app.injector.instanceOf[SnapshotWriter])
                       ),
                       "studies")
      Thread.sleep(400)
      actor
    }
  }

  describe("A studies processor must") {

    it("allow recovery from journal", PersistenceTest) {
      val study = factory.createDisabledStudy
      val cmd   = AddStudyCmd(sessionUserId = None, name = study.name, description = study.description)
      ask(studiesProcessor, cmd).mapTo[ServiceValidation[StudyEvent]].futureValue mustSucceed {
        _.eventType.isAdded must be(true)
      }
      studyRepository.getValues.map { s =>
        s.name
      } must contain(study.name)

      studyRepository.removeAll
      studiesProcessor = restartProcessor(studiesProcessor).futureValue

      studyRepository.getValues.size must be(1)
      studyRepository.getValues.map { s =>
        s.name
      } must contain(study.name)
    }

    it("recovers a snapshot", PersistenceTest) {
      val snapshotFilename = "testfilename"
      val studies = (1 to 2).map { _ =>
        factory.createDisabledStudy
      }
      val snapshotStudy = studies(1)
      val snapshotState = StudiesProcessor.SnapshotState(Set(snapshotStudy))

      Mockito.when(snapshotWriterMock.save(anyString, anyString)).thenReturn(snapshotFilename);
      Mockito
        .when(snapshotWriterMock.load(snapshotFilename))
        .thenReturn(Json.toJson(snapshotState).toString);

      studies.foreach(studyRepository.put)
      (studiesProcessor ? "snap").mapTo[String].futureValue

      studyRepository.removeAll
      studiesProcessor = restartProcessor(studiesProcessor).futureValue

      studyRepository.getValues.size must be(1)
      studyRepository.getByKey(snapshotStudy.id) mustSucceed { repoStudy =>
        repoStudy.name must be(snapshotStudy.name)
        ()
      }
    }

  }

}
