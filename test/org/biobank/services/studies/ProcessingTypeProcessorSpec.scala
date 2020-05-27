package org.biobank.services.studies

import akka.actor._
import akka.pattern._
import javax.inject.{Inject, Named}
import org.biobank.Global
import org.biobank.domain.ConcurrencySafeEntity
import org.biobank.domain.studies._
import org.biobank.fixtures._
import org.biobank.infrastructure.commands.ProcessingTypeCommands
import org.biobank.services._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import org.slf4j.LoggerFactory
import play.api.libs.json._
import scala.concurrent.duration._
import scalaz.Scalaz._

/*
 * This code helped a lot to write these tests:
 *
 * https://github.com/dnvriend/akka-persistence-inmemory-test/blob/master/src/main/scala/com/github/dnvriend/HelloWorld.scala
 */

case class NamedProcessingTypeProcessor @Inject()(@Named("processingType") processor: ActorRef)

class ProcessingTypesProcessorSpec extends ProcessorTestFixture with ProcessingTypeFixtures {

  import org.biobank.TestUtils._
  import org.scalatest.matchers.must.Matchers._
  import org.biobank.infrastructure.commands.ProcessingTypeCommands._
  import org.biobank.infrastructure.events.ProcessingTypeEvents._

  val log = LoggerFactory.getLogger(this.getClass)

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  // processor is recreated in these tests
  var processingTypeProcessor =
    app.injector.instanceOf[NamedProcessingTypeProcessor].processor

  val studyRepository = app.injector.instanceOf[StudyRepository]

  val collectionEventTypeRepository = app.injector.instanceOf[CollectionEventTypeRepository]

  val processingTypeRepository = app.injector.instanceOf[ProcessingTypeRepository]

  val nameGenerator = new NameGenerator(this.getClass)

  override def beforeEach() = {
    studyRepository.removeAll
    collectionEventTypeRepository.removeAll
    processingTypeRepository.removeAll
    super.beforeEach()
  }

  private def addToRepository[T <: ConcurrencySafeEntity[_]](entity: T): Unit =
    entity match {
      case s: Study               => studyRepository.put(s)
      case e: CollectionEventType => collectionEventTypeRepository.put(e)
      case e: ProcessingType      => processingTypeRepository.put(e)
      case e => fail(s"cannot add entity: $e")
    }

  private def collectedSpecimenDefinitionFixtures() = {
    val f = new CollectedSpecimenDefinitionFixtures()
    Set(f.study, f.collectionEventType)
      .foreach(addToRepository)
    f
  }

  private def restartProcessor(processor: ActorRef) = {
    gracefulStop(processor, 5 seconds, PoisonPill).map { _ =>
      val actor = system.actorOf(
        Props(
          new ProcessingTypeProcessor(app.injector.instanceOf[ProcessingTypeRepository],
                                      app.injector.instanceOf[CollectionEventTypeRepository],
                                      app.injector.instanceOf[SnapshotWriter])
        ),
        "processing-type-processor-id-2"
      )
      Thread.sleep(100)
      actor
    }
  }

  describe("A processingTypes processor must") {

    it("allow recovery from journal", PersistenceTest) {
      val f = collectedSpecimenDefinitionFixtures
      val cmdInput = ProcessingTypeCommands
        .InputSpecimenProcessing(expectedChange = f.processingType.input.expectedChange,
                                 count          = f.processingType.input.count,
                                 containerTypeId = f.processingType.input.containerTypeId
                                   .map(_.id),
                                 definitionType       = ProcessingType.collectedDefinition.id,
                                 entityId             = f.processingType.input.entityId.toString,
                                 specimenDefinitionId = f.processingType.input.specimenDefinitionId.id)

      val specimenDefinition = f.processingType.output.specimenDefinition
      val cmdSpecimenDefintition = ProcessingTypeCommands
        .SpecimenDefinition(name                    = specimenDefinition.name,
                            description             = specimenDefinition.description,
                            units                   = specimenDefinition.units,
                            anatomicalSourceType    = specimenDefinition.anatomicalSourceType,
                            preservationType        = specimenDefinition.preservationType,
                            preservationTemperature = specimenDefinition.preservationTemperature,
                            specimenType            = specimenDefinition.specimenType)

      val output = f.processingType.output
      val cmdOutput =
        ProcessingTypeCommands.OutputSpecimenProcessing(expectedChange     = output.expectedChange,
                                                        count              = output.count,
                                                        containerTypeId    = output.containerTypeId.map(_.id),
                                                        specimenDefinition = cmdSpecimenDefintition)

      val cmd = AddProcessingTypeCmd(sessionUserId = Global.DefaultUserId.id,
                                     studyId     = f.processingType.studyId.id,
                                     name        = f.processingType.name,
                                     description = f.processingType.description,
                                     enabled     = f.processingType.enabled,
                                     input       = cmdInput,
                                     output      = cmdOutput)

      val v = ask(processingTypeProcessor, cmd)
        .mapTo[ServiceValidation[ProcessingTypeEvent]]
        .futureValue

      v.isSuccess must be(true)
      processingTypeRepository.getValues.map { pt =>
        pt.name
      } must contain(f.processingType.name)

      processingTypeRepository.removeAll
      processingTypeProcessor = restartProcessor(processingTypeProcessor).futureValue

      processingTypeRepository.getValues.size must be(1)
      processingTypeRepository.getValues.map { pt =>
        pt.name
      } must contain(f.processingType.name)
    }

    it("recovers a snapshot", PersistenceTest) {
      val snapshotFilename = "testfilename"
      val fixtures = (1 to 2).map { _ =>
        collectedSpecimenDefinitionFixtures
      }
      val processingTypes = fixtures.map { f =>
        f.processingType
      }
      val snapshotProcessingType = processingTypes(1)
      val snapshotState          = ProcessingTypeProcessor.SnapshotState(Set(snapshotProcessingType))

      Mockito.when(snapshotWriterMock.save(anyString, anyString)).thenReturn(snapshotFilename);
      Mockito
        .when(snapshotWriterMock.load(snapshotFilename))
        .thenReturn(Json.toJson(snapshotState).toString);

      processingTypes.foreach(processingTypeRepository.put)

      (processingTypeProcessor ? "snap").mapTo[String].futureValue

      processingTypeRepository.removeAll
      processingTypeProcessor = restartProcessor(processingTypeProcessor).futureValue

      processingTypeRepository.getValues.size must be(1)
      processingTypeRepository
        .getByKey(snapshotProcessingType.id)
        .mustSucceed { repoProcessingType =>
          repoProcessingType.name must be(snapshotProcessingType.name)
        }

      ()
    }

  }

}
