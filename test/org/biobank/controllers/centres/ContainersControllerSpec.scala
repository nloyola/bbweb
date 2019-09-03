package org.biobank.controllers.centres

import org.biobank.controllers.PagedResultsSharedSpec
import org.biobank.domain._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.containers._
import org.biobank.dto._
import org.biobank.fixtures.{ControllerFixture, Url}
import org.biobank.matchers.PagedResultsMatchers
import play.api.libs.json._
import play.api.test.Helpers._

/**
 * Tests the REST API for [[Container]]s.
 */
class ContainersControllerSpec
    extends ControllerFixture with PagedResultsSharedSpec with PagedResultsMatchers {

  import org.biobank.TestUtils._
  import org.biobank.matchers.JsonMatchers._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._

  protected val basePath = "centres/containers"

  describe("Centre REST API") {

    describe("POST /api/containers") {

      describe("for root containers") {

        val url = uri("")

        it("add a root container") {
          val f = new RootContainerFixtures
          Set(f.centre, f.containerType).foreach(addToRepository)
          val addJson = containerToAddJson(f.container, f.containerType, f.centre.locations.toSeq(0))
          val reply   = makeAuthRequest(POST, url, addJson).value
          reply must beOkResponseWithJsonReply

          validateContainer(f.container, contentAsJson(reply))
        }

        it("adding a root container fails if centre is not defined") {
          val f       = new RootContainerFixtures
          val addJson = containerToAddJson(f.container, f.containerType, f.centre.locations.toSeq(0))
          val reply   = makeAuthRequest(POST, url, addJson).value
          reply must beNotFoundWithMessage("IdNotFound: centre id")
        }

        it("adding a root container fails if container type is not defined") {
          val f = new RootContainerFixtures
          centreRepository.put(f.centre)
          val addJson = containerToAddJson(f.container, f.containerType, f.centre.locations.toSeq(0))
          val reply   = makeAuthRequest(POST, url, addJson).value
          reply must beNotFoundWithMessage("IdNotFound: container type id")
        }

        it("adding a root container fails if a container type is for a specimen container") {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerType,
              f.storageContainerType,
              f.specimenContainerType,
              f.rootContainer,
              f.storageContainer,
              f.specimenContainer)
            .foreach(addToRepository)
          val addJson =
            containerToAddJson(f.rootContainer, f.specimenContainerType, f.centre.locations.toSeq(0))
          val reply = makeAuthRequest(POST, url, addJson).value
          reply must beBadRequestWithMessage("InvalidStatus: not a storage container type")
        }

        it("adding a root container fails if a container with the inventory ID already exists") {
          val f = new RootContainerFixtures
          Set(f.centre, f.containerType, f.container).foreach(addToRepository)
          val addJson = containerToAddJson(f.container, f.containerType, f.centre.locations.toSeq(0))
          val reply   = makeAuthRequest(POST, url, addJson).value
          reply must beForbiddenRequestWithMessage(
            "EntityCriteriaError: container with inventory ID already exists"
          )
        }

      }

      describe("for storage containers") {

        val url = uri("add-storage")

        val setupFull = () => {
          val f = new StorageContainerFixtures
          Set(f.centre, f.rootContainerSchema, f.rootContainerType, f.storageContainerType, f.rootContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.storageContainerType, f.storageContainer)
        }

        val setupMissingContainerType = () => {
          val f = new StorageContainerFixtures
          Set(f.centre, f.storageContainerSchema, f.rootContainerType, f.rootContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.storageContainerType, f.storageContainer)
        }

        val setupInvalidContainerType = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerType,
              f.rootContainerSchema,
              f.specimenContainerSchema,
              f.storageContainerType,
              f.specimenContainerType,
              f.rootContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.specimenContainerType, f.storageContainer)
        }

        val setupMissingParent = () => {
          val f = new StorageContainerFixtures
          Set(f.centre, f.rootContainerType, f.rootContainerType).foreach(addToRepository)
          ChildContainerTestSetup(url, f.storageContainerType, f.storageContainer)
        }

        val setupInventoryIdExists = () => {
          val f = new StorageContainerFixtures
          Set(f.centre,
              f.rootContainerType,
              f.storageContainerSchema,
              f.storageContainerType,
              f.rootContainer,
              f.storageContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.storageContainerType, f.storageContainer)
        }

        val setupPositionNotAvailable = () => {
          val f = new StorageContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.storageContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.rootContainer,
              f.storageContainer)
            .foreach(addToRepository)
          val container = f.storageContainer.copy(inventoryId = nameGenerator.next[String])
          ChildContainerTestSetup(url, f.storageContainerType, container)
        }

        childContainerSharedBehaviour(setupFull,
                                      setupMissingContainerType,
                                      setupInvalidContainerType,
                                      setupMissingParent,
                                      setupInventoryIdExists,
                                      setupPositionNotAvailable)
      }

      describe("for specimen containers") {

        val url = uri("add-specimen")

        val setupFull = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.storageContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.specimenContainerType,
              f.rootContainer,
              f.storageContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.specimenContainerType, f.specimenContainer)
        }

        val setupMissingContainerType = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.storageContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.rootContainer,
              f.storageContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.specimenContainerType, f.specimenContainer)
        }

        val setupInvalidContainerType = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.storageContainerSchema,
              f.specimenContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.rootContainer,
              f.storageContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.storageContainerType, f.specimenContainer)
        }

        val setupMissingParent = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.specimenContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.rootContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.specimenContainerType, f.specimenContainer)
        }

        val setupInventoryIdExists = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.specimenContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.specimenContainerType,
              f.rootContainer,
              f.storageContainer,
              f.specimenContainer)
            .foreach(addToRepository)
          ChildContainerTestSetup(url, f.specimenContainerType, f.specimenContainer)
        }

        val setupPositionNotAvailable = () => {
          val f = new SpecimenContainerFixtures
          Set(f.centre,
              f.rootContainerSchema,
              f.storageContainerSchema,
              f.specimenContainerSchema,
              f.rootContainerType,
              f.storageContainerType,
              f.specimenContainerType,
              f.rootContainer,
              f.storageContainer,
              f.specimenContainer)
            .foreach(addToRepository)
          val container = f.specimenContainer.copy(inventoryId = nameGenerator.next[String])
          ChildContainerTestSetup(url, f.specimenContainerType, container)
        }

        childContainerSharedBehaviour(setupFull,
                                      setupMissingContainerType,
                                      setupInvalidContainerType,
                                      setupMissingParent,
                                      setupInventoryIdExists,
                                      setupPositionNotAvailable)

      }

    }

  }

  case class ChildContainerTestSetup(url: Url, containerType: ContainerType, container: ChildContainer)

  def childContainerSharedBehaviour(
      setupFull:                 () => ChildContainerTestSetup,
      setupMissingContainerType: () => ChildContainerTestSetup,
      setupInvalidContainerType: () => ChildContainerTestSetup,
      setupMissingParent:        () => ChildContainerTestSetup,
      setupInventoryIdExists:    () => ChildContainerTestSetup,
      setupPositionNotAvailable: () => ChildContainerTestSetup
    ) =
    describe("child container (shared)") {

      it("add a child container") {
        val (url, containerType, container) = ChildContainerTestSetup.unapply(setupFull()).get
        val addJson                         = containerToAddJson(container, containerType)
        val reply                           = makeAuthRequest(POST, url, addJson).value
        reply must beOkResponseWithJsonReply

        validateContainer(container, contentAsJson(reply))
      }

      it("adding a child container fails if container type is not defined") {
        val (url, containerType, container) = ChildContainerTestSetup.unapply(setupMissingContainerType()).get
        val addJson                         = containerToAddJson(container, containerType)
        val reply                           = makeAuthRequest(POST, url, addJson).value
        reply must beNotFoundWithMessage("IdNotFound: container type id")
      }

      it("adding a child container fails if a container type is of the wrong type") {
        val (url, containerType, container) = ChildContainerTestSetup.unapply(setupInvalidContainerType()).get
        val addJson                         = containerToAddJson(container, containerType)
        val reply                           = makeAuthRequest(POST, url, addJson).value

        val expectedMsg = container match {
          case c: StorageContainer  => "InvalidStatus: not a storage container type"
          case c: SpecimenContainer => "InvalidStatus: not a specimen container type"
        }
        reply must beBadRequestWithMessage(expectedMsg)
      }

      it("adding a child container fails if parent container is not defined") {
        val (url, containerType, container) = ChildContainerTestSetup.unapply(setupMissingParent()).get
        val addJson                         = containerToAddJson(container, containerType)
        val reply                           = makeAuthRequest(POST, url, addJson).value
        reply must beNotFoundWithMessage("IdNotFound: container id")
      }

      it("adding a child container fails if a container with the inventory ID already exists") {
        val (url, containerType, container) = ChildContainerTestSetup.unapply(setupInventoryIdExists()).get
        val addJson                         = containerToAddJson(container, containerType)
        val reply                           = makeAuthRequest(POST, url, addJson).value
        reply must beForbiddenRequestWithMessage(
          "EntityCriteriaError: container with inventory ID already exists"
        )
      }

      it("adding a child container fails if the position is not available") {
        val (url, containerType, container) = ChildContainerTestSetup.unapply(setupPositionNotAvailable()).get
        val addJson                         = containerToAddJson(container, containerType)
        val reply                           = makeAuthRequest(POST, url, addJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: position not empty at label")
      }

    }

  private class RootContainerFixtures {
    val location = factory.createLocation
    val centre   = factory.defaultEnabledCentre.copy(locations = Set(location))

    val containerSchema =
      factory.createContainerSchema.copy(positions = Set(factory.createContainerSchemaPosition))

    val containerType = factory.createStorageContainerType
    val container     = factory.createRootContainer(centre, containerType)
  }

  private class StorageContainerFixtures {
    private val f           = new RootContainerFixtures
    val rootContainerSchema = f.containerSchema
    val rootContainer       = f.container
    val rootContainerType   = f.containerType
    val centre              = f.centre

    val storageContainerSchema =
      factory.createContainerSchema.copy(positions = Set(factory.createContainerSchemaPosition))

    val storageContainerType = factory.createStorageContainerType(centre, storageContainerSchema)

    val storageContainer = factory.createStorageContainer(storageContainerType,
                                                          rootContainer,
                                                          rootContainerSchema.positions.headOption.value)
  }

  private class SpecimenContainerFixtures {
    private val f              = new StorageContainerFixtures
    val rootContainerSchema    = f.rootContainerSchema
    val rootContainer          = f.rootContainer
    val rootContainerType      = f.rootContainerType
    val centre                 = f.centre
    val storageContainerSchema = f.storageContainerSchema
    val storageContainerType   = f.storageContainerType
    val storageContainer       = f.storageContainer

    val specimenContainerSchema =
      factory.createContainerSchema.copy(positions = Set(factory.createContainerSchemaPosition))

    val specimenContainerType = factory.createSpecimenContainerType(specimenContainerSchema)

    val specimenContainer = factory.createSpecimenContainer(specimenContainerType,
                                                            storageContainer,
                                                            storageContainerSchema.positions.headOption.value)
  }

  def validateContainer[T <: Container, D <: ContainerDto](
      jsonContainer:   JsResult[D],
      container:       T
    )(updateWithNewId: ContainerId => (T, DomainValidation[T])
    ): Unit = {
    jsonContainer must be(jsSuccess)

    val newContainerId                    = ContainerId(jsonContainer.get.id)
    val (updatedContainer, repoContainer) = updateWithNewId(newContainerId)

    jsonContainer.get must matchDtoToContainer(updatedContainer)
    repoContainer mustSucceed { repoContainer =>
      repoContainer must matchContainer(updatedContainer)
    }
  }

  def validateContainer(container: RootContainer, json: JsValue): Unit = {
    val replyContainer = (json \ "data").validate[RootContainerDto]
    validateContainer(replyContainer, container) { id =>
      (container.copy(id = id), containerRepository.getRootContainer(id))
    }
  }

  def validateContainer(container: ChildContainer, json: JsValue): Unit =
    container match {
      case c: StorageContainer  => validateContainer(c, json)
      case c: SpecimenContainer => validateContainer(c, json)
    }

  def validateContainer(container: StorageContainer, json: JsValue): Unit = {
    val replyContainer = (json \ "data").validate[StorageContainerDto]
    validateContainer(replyContainer, container) { id =>
      (container.copy(id = id), containerRepository.getStorageContainer(id))
    }
  }

  def validateContainer(container: SpecimenContainer, json: JsValue): Unit = {
    val replyContainer = (json \ "data").validate[SpecimenContainerDto]
    validateContainer(replyContainer, container) { id =>
      (container.copy(id = id), containerRepository.getSpecimenContainer(id))
    }
  }

  private def containerToAddJson(
      container:     RootContainer,
      containerType: ContainerType,
      location:      Location
    ): JsValue =
    Json.obj("label"           -> container.label,
             "inventoryId"     -> container.inventoryId,
             "centreId"        -> containerType.centreId,
             "locationId"      -> location.id,
             "temperature"     -> container.temperature,
             "containerTypeId" -> containerType.id)

  private def containerToAddJson(container: ChildContainer, containerType: ContainerType): JsValue =
    Json.obj("inventoryId"     -> container.inventoryId,
             "containerTypeId" -> containerType.id,
             "parentId"        -> container.parentId,
             "label"           -> container.position.label)

}
