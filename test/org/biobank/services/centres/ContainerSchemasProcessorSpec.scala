package org.biobank.services.centres

import akka.actor._
import akka.pattern._
import javax.inject.{Inject, Named}
import org.biobank.fixtures._
import org.biobank.domain.centres.CentreRepository
import org.biobank.domain.containers.{ContainerSchemaRepository, ContainerTypeRepository}
import org.biobank.infrastructure.commands.ContainerSchemaCommands._
import org.biobank.infrastructure.events.ContainerSchemaEvents._
import org.biobank.services._
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import play.api.libs.json._
import scala.concurrent.duration._

case class NamedContainerSchemasProcessor @Inject()(@Named("containerSchemasProcessor") processor: ActorRef)

class ContainerSchemasProcessorSpec extends ProcessorTestFixture with PresistenceQueryEvents {

  import org.biobank.TestUtils._
  import org.scalatest.matchers.must.Matchers._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private var schemaProcessor = app.injector.instanceOf[NamedContainerSchemasProcessor].processor

  private val schemaRepository = app.injector.instanceOf[ContainerSchemaRepository]

  private val centreRepository = app.injector.instanceOf[CentreRepository]

  private val containerTypeRepository = app.injector.instanceOf[ContainerTypeRepository]

  private val nameGenerator = new NameGenerator(this.getClass)

  override def beforeEach() = {
    schemaRepository.removeAll
    super.beforeEach()
  }

  private def restartProcessor(processor: ActorRef) = {
    gracefulStop(processor, 5 seconds, PoisonPill).map { _ =>
      val actor = system.actorOf(
        Props(
          new ContainerSchemasProcessor(schemaRepository,
                                        centreRepository,
                                        containerTypeRepository,
                                        app.injector.instanceOf[SnapshotWriter])
        ),
        "containerSchemas"
      )
      Thread.sleep(400)
      actor
    }
  }

  describe("A containerSchema processor must") {

    it("allow recovery from journal", PersistenceTest) {
      val schema = factory.createContainerSchema
      val cmd = AddContainerSchemaCmd(sessionUserId = nameGenerator.next[String],
                                      name        = schema.name,
                                      description = schema.description,
                                      shared      = schema.shared,
                                      centreId    = schema.centreId.id,
                                      labels      = schema.labels.toList)
      ask(schemaProcessor, cmd).mapTo[ServiceValidation[ContainerSchemaEvent]].futureValue mustSucceed {
        _.eventType.isAdded must be(true)
      }
      schemaRepository.getValues.map { c =>
        c.name
      } must contain(schema.name)

      schemaRepository.removeAll
      schemaProcessor = restartProcessor(schemaProcessor).futureValue

      schemaRepository.getValues.map { c =>
        c.name
      } must contain(schema.name)
    }

    it("accept a snapshot offer", PersistenceTest) {
      val snapshotFilename = "testfilename"
      val schemas = (1 to 2).map { _ =>
        factory.createContainerSchema
      }
      val snapshotContainerSchema = schemas(1)
      val snapshotState           = ContainerSchemasProcessor.SnapshotState(Set(snapshotContainerSchema))

      Mockito.when(snapshotWriterMock.save(anyString, anyString)).thenReturn(snapshotFilename);
      val snapshotStateJson = Json.toJson(snapshotState).toString
      Mockito
        .when(snapshotWriterMock.load(snapshotFilename))
        .thenReturn(snapshotStateJson);

      schemas.foreach(schemaRepository.put)
      (schemaProcessor ? "snap").mapTo[String].futureValue

      schemaRepository.removeAll
      schemaProcessor = restartProcessor(schemaProcessor).futureValue

      schemaRepository.getByKey(snapshotContainerSchema.id) mustSucceed { repoContainerSchema =>
        repoContainerSchema.name must be(snapshotContainerSchema.name)
        ()
      }
    }

  }

}
