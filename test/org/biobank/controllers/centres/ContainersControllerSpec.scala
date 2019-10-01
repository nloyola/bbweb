package org.biobank.controllers.centres

import java.time.OffsetDateTime
import org.biobank.controllers.PagedResultsSharedSpec
import org.biobank.domain._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.containers._
import org.biobank.dto._
import org.biobank.fixtures._
import org.biobank.matchers.PagedResultsMatchers
import play.api.libs.json._
import play.api.test.Helpers._

class RootContainersControllerSpec
    extends CommonStorageContainerControllerSpec[RootContainer, StorageContainerType, RootContainerFixture] {

  protected def addUrl(): Url = uri("root")

  protected def fixture(): RootContainerFixture = RootContainerFixture(factory)

  describe("GET /api/centres/containers/:centreId") {

    describe("list a single root container") {
      listSingle() { () =>
        val f = fixture
        f.allEntities.foreach(addToRepository)
        (uri("search", f.getCentre.id.id), f.container)
      }
    }

    describe("get all root contaniers for a centre") {
      listMultiple() { () =>
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val sibling = f.createSiblingContainer(factory)
        addToRepository(sibling)

        (uri("search", f.centre.id.id), List(f.container, sibling))
      }
    }

    describe("list root contaneirs sorted by label") {

      describe("in ascending order") {
        listMultiple() { () =>
          val f = fixture
          f.allEntities.foreach(addToRepository)
          val sibling = f.createSiblingContainer(factory)
          addToRepository(sibling)

          val containers = List(f.container, sibling).sortWith(_.label < _.label)
          (uri("search", f.centre.id.id).addQueryString("sort=label"), containers)
        }
      }

      describe("in descending order") {
        listMultiple() { () =>
          val f = fixture
          f.allEntities.foreach(addToRepository)
          val sibling = f.createSiblingContainer(factory)
          addToRepository(sibling)

          val containers = List(f.container, sibling).sortWith(_.label > _.label)
          (uri("search", f.centre.id.id).addQueryString("sort=-label"), containers)
        }
      }
    }
  }

  describe("POST /api/containers") {

    it("adding a root container fails if centre is not defined") {
      val f       = fixture
      val addJson = containerToAddJson(f)
      val reply   = makeAuthRequest(POST, addUrl, addJson).value
      reply must beNotFoundWithMessage("IdNotFound: centre id")
    }

    it("adding a root container fails if a container type is for a specimen container") {
      val f = fixture
      f.allEntitiesButContainer.foreach(addToRepository)

      val specimenContainerType = factory.createSpecimenContainerType
      addToRepository(specimenContainerType)

      val container =
        f.container.withContainerType(specimenContainerType.id, OffsetDateTime.now).toOption.value
      val addJson = containerToAddJson(container, fixture)
      val reply   = makeAuthRequest(POST, addUrl, addJson).value
      reply must beBadRequestWithMessage("InvalidStatus: not a storage container type")
    }

  }

  describe("POST /api/centres/containers/enabled/:id") {

    it("update a container's enabled state") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val newValue   = !f.container.enabled
      val updateJson = Json.obj("expectedVersion" -> Some(f.container.version), "enabled" -> newValue)

      val reply = makeAuthRequest(POST, uri("enabled", f.container.id.id), updateJson).value
      reply must beOkResponseWithJsonReply
      val updatedContainer = f.container.withEnabled(newValue, OffsetDateTime.now).toOption.value
      validateContainer(updatedContainer, contentAsJson(reply))
    }

    describe("fail when updating inventory ID with invalid version") {
      updateWithInvalidVersionSharedBehaviour { container =>
        (uri("enabled", container.id.id), Json.obj("enabled" -> !container.enabled))
      }
    }
  }

  describe("POST /api/centres/containers/location/:id") {

    it("update a container's location") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val centre   = factory.createEnabledCentre
      val location = centre.locations.headOption.value
      addToRepository(centre)

      val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                "centreId"   -> centre.id.id,
                                "locationId" -> location.id.id)

      val reply = makeAuthRequest(POST, uri("location", f.container.id.id), updateJson).value
      reply must beOkResponseWithJsonReply
      val updatedContainer =
        f.container.withCentreLocation(centre.id, location.id, OffsetDateTime.now).toOption.value
      validateContainer(updatedContainer, contentAsJson(reply))
    }

    it("fail when updating a container's location and centre does not exist") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val centre   = factory.createEnabledCentre
      val location = centre.locations.headOption.value

      val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                "centreId"   -> centre.id.id,
                                "locationId" -> location.id.id)

      val reply = makeAuthRequest(POST, uri("location", f.container.id.id), updateJson).value
      reply must beNotFoundWithMessage("IdNotFound: centre id")
    }

    it("fail when updating a container's location and location is invalid") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val centre     = factory.createEnabledCentre
      val locationId = nameGenerator.next[Container]
      addToRepository(centre)

      val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                "centreId"   -> centre.id.id,
                                "locationId" -> locationId)

      val reply = makeAuthRequest(POST, uri("location", f.container.id.id), updateJson).value
      reply must beNotFoundWithMessage("IdNotFound: invalid location id")
    }

    describe("fail when updating location with invalid version") {
      updateWithInvalidVersionSharedBehaviour { container =>
        val f = fixture
        (uri("location", container.id.id),
         Json.obj("centreId" -> f.centre.id.id, "locationId" -> f.centre.locations.headOption.value.id.id))
      }
    }
  }

  describe("POST /api/centres/containers/temperature/:id") {

    it("update a container's temperature property") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val temperature = PreservationTemperature.Plus4celcius
      val updateJson =
        Json.obj("expectedVersion" -> Some(f.container.version), "temperature" -> temperature.toString)
      val reply = makeAuthRequest(POST, uri("temperature", f.container.id.id), updateJson).value
      reply must beOkResponseWithJsonReply
      val updatedContainer =
        f.container.withTemperature(temperature, OffsetDateTime.now).toOption.value
      validateContainer(updatedContainer, contentAsJson(reply))
    }
  }

  describe("POST /api/centres/containers/position/:id") {

    it("changing position on a root container fails") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                "parentId" -> nameGenerator.next[Container],
                                "label"    -> nameGenerator.next[Container])
      val reply = makeAuthRequest(POST, uri("position", f.container.id.id), updateJson).value
      reply must beBadRequestWithMessage("EntityCriteriaError: cannot update position on a root container")
    }

  }

  protected def containerToAddJson(container: RootContainer, fixture: RootContainerFixture): JsValue = {
    val location = fixture.centre.locations.headOption.value
    Json.obj("label"           -> container.label,
             "inventoryId"     -> container.inventoryId,
             "centreId"        -> fixture.containerType.centreId,
             "locationId"      -> location.id,
             "temperature"     -> container.temperature,
             "containerTypeId" -> container.containerTypeId)
  }
}

class StorageContainersControllerSpec
    extends ChildContainerControllerSpec[StorageContainer,
                                         StorageContainerType,
                                         RootContainer,
                                         StorageContainerFixture] with CommonStorageContainerControllerSpec[
      StorageContainer,
      StorageContainerType,
      StorageContainerFixture
    ] {

  protected def addUrl(): Url = uri("storage")

  protected def fixture(): StorageContainerFixture = StorageContainerFixture(factory)

  describe("POST /api/containers") {

    it("adding a child container fails if a container type is of the wrong type") {
      val f = fixture
      f.allEntitiesButContainer.foreach(addToRepository)

      val specimenContainerType = factory.createSpecimenContainerType
      addToRepository(specimenContainerType)

      val container: StorageContainer =
        f.container.withContainerType(specimenContainerType.id, OffsetDateTime.now).toOption.value

      val addJson = containerToAddJson(container, f)
      val reply   = makeAuthRequest(POST, addUrl, addJson).value
      reply must beBadRequestWithMessage("InvalidStatus: not a storage container type")
    }

  }

}

class SpecimenContainersControllerSpec
    extends ChildContainerControllerSpec[SpecimenContainer,
                                         SpecimenContainerType,
                                         StorageContainer,
                                         SpecimenContainerFixture] {

  protected def addUrl(): Url = uri("specimen")

  protected def fixture(): SpecimenContainerFixture = SpecimenContainerFixture(factory)

  describe("POST /api/containers") {

    it("adding a child container fails if a container type is of the wrong type") {
      val f = fixture
      f.allEntitiesButContainer.foreach(addToRepository)

      val storageContainerType = factory.createStorageContainerType
      addToRepository(storageContainerType)

      val container =
        f.container.withContainerType(storageContainerType.id, OffsetDateTime.now).toOption.value

      val addJson = containerToAddJson(container, f)
      val reply   = makeAuthRequest(POST, addUrl, addJson).value
      reply must beBadRequestWithMessage("InvalidStatus: not a specimen container type")
    }

  }

  describe("POST /api/centres/containers/constraints/:id") {

    it("fails when adding constraints to a specimen container") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val constraints = factory.createContainerConstraints
      val updateJson =
        Json.obj("expectedVersion" -> Some(f.container.version)) ++ constraintsToJson(constraints)
      val reply = makeAuthRequest(POST, uri("constraints", f.container.id.id), updateJson).value
      reply must beBadRequestWithMessage(
        "EntityCriteriaError: cannot add constraints to a specimen container"
      )
    }

    it("fails when removing constraints on a specimen container") {
      val f = fixture
      f.allEntities.foreach(addToRepository)

      val updateJson =
        Json.obj("expectedVersion" -> Some(f.container.version))
      val reply = makeAuthRequest(POST, uri("constraints/remove", f.container.id.id), updateJson).value
      reply must beBadRequestWithMessage(
        "EntityCriteriaError: cannot remove constraints on a specimen container"
      )
    }

  }

}

trait ChildContainerControllerSpec[
    C <: ChildContainer,
    T <: ContainerType,
    PC <: Container,
    F <: ChildContainerFixture[C, T, PC]]
    extends CommonContainerControllerSpec[C, T, F] {

  describe("(child common)") {

    describe("POST /api/containers") {

      it("adding a child container fails if parent container is not defined") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        containerRepository.remove(f.parent.container)
        val addJson = containerToAddJson(f)
        val reply   = makeAuthRequest(POST, addUrl, addJson).value
        reply must beNotFoundWithMessage("IdNotFound: container id")
      }

      it("adding a child container fails if the label is occupied") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        val container = f.containerWithInventoryId(nameGenerator.next[Container])
        val addJson   = containerToAddJson(container, f)
        val reply     = makeAuthRequest(POST, addUrl, addJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: position is occupied at label")
      }

      it("adding a child container fails if the label is invalid") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        val container = f.containerWithLabel(nameGenerator.next[Container])
        val addJson   = containerToAddJson(container, f)
        val reply     = makeAuthRequest(POST, addUrl, addJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: label is invalid")
      }

    }

    describe("POST /api/centres/containers/label/:id") {

      it("update a container's label") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val newLabel   = f.parent.schema.labels.toSeq(1)
        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version), "label" -> newLabel)
        val reply      = makeAuthRequest(POST, uri("label", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer = f.container.withLabel(newLabel).toOption.value

        validateContainer(updatedContainer, contentAsJson(reply))
      }

      it("fail if new label is already occupied") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val sibling = f.createSiblingContainer(factory)
        addToRepository(sibling)

        val newLabel   = sibling.schemaLabel.label
        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version), "label" -> newLabel)
        val reply      = makeAuthRequest(POST, uri("label", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: position is occupied at label")
      }

      it("fail if label is invalid") {
        val newLabel = nameGenerator.next[Container]
        val f        = fixture
        f.allEntities.foreach(addToRepository)

        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version), "label" -> newLabel)
        val reply      = makeAuthRequest(POST, uri("label", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: label is invalid on schema")
      }
    }

    describe("POST /api/centres/containers/location/:id") {

      it("fail when updating a container's location") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val centre   = factory.createEnabledCentre
        val location = centre.locations.headOption.value
        addToRepository(centre)

        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                  "centreId"   -> centre.id.id,
                                  "locationId" -> location.id.id)

        val reply = makeAuthRequest(POST, uri("location", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage(
          "EntityCriteriaError: cannot change the centre location on a non root container"
        )
      }
    }

    describe("POST /api/centres/containers/temperature/:id") {

      it("fail when updating a container's temperature property") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val temperature = PreservationTemperature.Plus4celcius
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version), "temperature" -> temperature.toString)
        val reply = makeAuthRequest(POST, uri("temperature", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage(
          "EntityCriteriaError: cannot change the temperature on a non root container"
        )
      }
    }

    describe("POST /api/centres/containers/position/:id") {

      it("update a container's position") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val parent = f.createParentContainer(factory)
        addToRepository(parent)

        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                  "parentId" -> parent.id,
                                  "label"    -> f.parent.schema.labels.toSeq(1))
        val reply = makeAuthRequest(POST, uri("position", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer = f.container
          .withPosition(parent.id, f.parent.schema.labels.toSeq(1), OffsetDateTime.now).toOption.value

        validateContainer(updatedContainer, contentAsJson(reply))
      }

      it("fails when poisiton is the same as current") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                  "parentId" -> f.container.parentId,
                                  "label"    -> f.container.schemaLabel.label)
        val reply = makeAuthRequest(POST, uri("position", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage(
          "EntityCriteriaError: container already occupies requested position"
        )
      }

      it("fails when position is occupied") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val sibling = f.createSiblingContainer(factory)
        addToRepository(sibling)

        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                  "parentId" -> sibling.parentId,
                                  "label"    -> sibling.schemaLabel.label)
        val reply = makeAuthRequest(POST, uri("position", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: position is occupied at label")
      }

      it("fails when position label is invalid") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version),
                                  "parentId" -> f.container.parentId,
                                  "label"    -> nameGenerator.next[Container])
        val reply = makeAuthRequest(POST, uri("position", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: label is invalid on schema")
      }

    }

  }

  protected def containerToAddJson(container: C, fixture: F): JsValue = {
    Json.obj("inventoryId"     -> container.inventoryId,
             "containerTypeId" -> container.containerTypeId,
             "parentId"        -> fixture.parent.container.id,
             "label"           -> container.schemaLabel.label)
  }

}

trait CommonStorageContainerControllerSpec[
    C <: Container with HasEnabled with HasConstraints,
    T <: ContainerType,
    F <: ContainerFixture[C, T]]
    extends CommonContainerControllerSpec[C, T, F] {

  import org.biobank.matchers.JsonMatchers._
  import org.biobank.matchers.DtoMatchers._

  describe("(root and storage container common)") {

    describe("GET /api/centres/containers/children/:slug") {

      it("gets container's children ") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val children = (1 to 2)
          .map { _ =>
            val (childContainer, childContainerType, childSchema) = f.createChild(factory).value
            Set(childContainer, childContainerType, childSchema).foreach(addToRepository)
            childContainer
          }.sortBy(_.id.id)

        val reply = makeAuthRequest(GET, uri("children", f.container.slug.id)).value
        reply must beOkResponseWithJsonReply

        val json      = contentAsJson(reply)
        val replyInfo = (json \ "data").validate[ContainerChildrenInfo]
        replyInfo must be(jsSuccess)

        replyInfo.get.container must matchDtoToContainerInfo(f.container)

        (replyInfo.get.children.toList.sortBy(_.id) zip children).foreach {
          case (replyChild, child) =>
            replyChild must matchDtoToContainerInfo(child)
        }
      }

      it("fails to get container's children if container does not exist") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)

        val reply = makeAuthRequest(GET, uri("children", f.container.slug.id)).value
        reply must beNotFoundWithMessage("EntityCriteriaNotFound: container slug")
      }
    }

    describe("POST /api/centres/containers/enabled/:id") {

      it("update a container's enabled state") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val newValue   = !f.container.enabled
        val updateJson = Json.obj("expectedVersion" -> Some(f.container.version), "enabled" -> newValue)

        val reply = makeAuthRequest(POST, uri("enabled", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer = f.container.withEnabled(newValue, OffsetDateTime.now).toOption.value
        validateContainer(updatedContainer, contentAsJson(reply))
      }

      describe("fail when updating enabled property with invalid version") {
        updateWithInvalidVersionSharedBehaviour { container =>
          (uri("enabled", container.id.id), Json.obj("enabled" -> !container.enabled))
        }
      }
    }

    describe("POST /api/centres/containers/constraints/:id") {

      it("add constraints to a container") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val constraints = factory.createContainerConstraints
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version)) ++ constraintsToJson(constraints)
        val reply = makeAuthRequest(POST, uri("constraints", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer =
          f.container.withConstraints(Some(constraints), OffsetDateTime.now).toOption.value
        validateContainer(updatedContainer, contentAsJson(reply))
      }

      it("remove constraints on a container") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version))
        val reply = makeAuthRequest(POST, uri("constraints/remove", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer = f.container.withConstraints(None, OffsetDateTime.now).toOption.value
        validateContainer(updatedContainer, contentAsJson(reply))
      }

      it("fails when adding constraints to a container that does not exist") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)

        val constraints = factory.createContainerConstraints
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version)) ++ constraintsToJson(constraints)
        val reply = makeAuthRequest(POST, uri("constraints", f.container.id.id), updateJson).value
        reply must beNotFoundWithMessage("IdNotFound: container id")
      }

      it("fails when removing constraints on a container that does not exit") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)

        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version))
        val reply = makeAuthRequest(POST, uri("constraints/remove", f.container.id.id), updateJson).value
        reply must beNotFoundWithMessage("IdNotFound: container id")
      }

      describe("fail when updating constraints with invalid version") {
        updateWithInvalidVersionSharedBehaviour { container =>
          val constraints = factory.createContainerConstraints
          (uri("constraints", container.id.id),
           Json.obj("enabled" -> !container.enabled) ++ constraintsToJson(constraints))
        }
      }

      describe("fail when removing constraints with invalid version") {
        updateWithInvalidVersionSharedBehaviour { container =>
          (uri("constraints/remove", container.id.id), Json.obj())
        }
      }
    }
  }
}

trait CommonContainerControllerSpec[C <: Container, T <: ContainerType, F <: ContainerFixture[C, T]]
    extends ControllerFixture with PagedResultsSharedSpec with PagedResultsMatchers {

  import org.biobank.TestUtils._
  import org.biobank.matchers.JsonMatchers._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._

  protected val basePath = "centres/containers"

  protected def addUrl(): Url

  protected def fixture(): F

  protected def containerToAddJson(container: C, fixture: F): JsValue

  protected def containerToAddJson(fixture: F): JsValue = containerToAddJson(fixture.container, fixture)

  describe("(common)") {

    describe("GET /api/centres/containers/:slug") {

      it("get a single container by slug") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        val reply = makeAuthRequest(GET, uri(f.container.slug.id)).value
        reply must beOkResponseWithJsonReply
        validateContainer(f.container, contentAsJson(reply))
      }

      it("fail when querying for a single container and slug is invalid") {
        val f     = fixture
        val reply = makeAuthRequest(GET, uri(f.container.slug.id)).value
        reply must beNotFoundWithMessage("EntityCriteriaNotFound: container slug")
      }
    }

    describe("GET /api/centres/containers/:centreId") {

      it("list none") {
        val centre = factory.createEnabledCentre
        addToRepository(centre)
        uri("search", centre.id.id) must beEmptyResults
      }

    }

    describe("POST /api/containers") {

      it("add a container") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        val addJson = containerToAddJson(f)
        val reply   = makeAuthRequest(POST, addUrl, addJson).value
        reply must beOkResponseWithJsonReply

        validateContainer(f.container, contentAsJson(reply))
      }

      it("adding a container fails if container type is not defined") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        containerTypeRepository.remove(f.containerType)
        val addJson = containerToAddJson(f)
        val reply   = makeAuthRequest(POST, addUrl, addJson).value
        reply must beNotFoundWithMessage("IdNotFound: container type id")
      }

      it("adding a container fails if a container with the inventory ID already exists") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        val addJson = containerToAddJson(f)
        val reply   = makeAuthRequest(POST, addUrl, addJson).value
        reply must beForbiddenRequestWithMessage(
          "EntityCriteriaError: container with inventory ID already exists"
        )
      }

    }

    describe("POST /api/centres/containers/inventoryId/:id") {

      it("update a container's inventoryId") {
        val newInventoryId = nameGenerator.next[Container]
        val f              = fixture
        f.allEntities.foreach(addToRepository)
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version), "inventoryId" -> newInventoryId)
        val reply = makeAuthRequest(POST, uri("inventoryId", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer =
          f.container.withInventoryId(newInventoryId, Slug(newInventoryId)).toOption.value
        validateContainer(updatedContainer, contentAsJson(reply))
      }

      describe("fail when updating inventory ID with invalid version") {
        updateWithInvalidVersionSharedBehaviour { container =>
          (uri("inventoryId", container.id.id), Json.obj("inventoryId" -> nameGenerator.next[Container]))
        }
      }
    }

    describe("POST /api/centres/containers/label/:id") {

      describe("fail when updating label with invalid version") {
        updateWithInvalidVersionSharedBehaviour { container =>
          (uri("label", container.id.id), Json.obj("label" -> nameGenerator.next[Container]))
        }
      }
    }

    describe("POST /api/centres/containers/containerTypeId/:id") {

      it("change a container's container type") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val containerType = f.createContainerType(factory)
        addToRepository(containerType)

        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version), "containerTypeId" -> containerType.id)
        val reply = makeAuthRequest(POST, uri("containerType", f.container.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val updatedContainer =
          f.container.withContainerType(containerType.id, OffsetDateTime.now).toOption.value
        validateContainer(updatedContainer, contentAsJson(reply))
      }

      it("fails when changing a container's container type to one that does not exist") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val containerType = f.createContainerType(factory)
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version), "containerTypeId" -> containerType.id)
        val reply = makeAuthRequest(POST, uri("containerType", f.container.id.id), updateJson).value
        reply must beNotFoundWithMessage("IdNotFound: container type id")
      }

      it("fails when changing a container's container type to one of the wrong type") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val containerType = f.container match {
          case _: RootContainer | _: StorageContainer => factory.createSpecimenContainerType
          case c: SpecimenContainer => factory.createStorageContainerType
        }
        addToRepository(containerType)

        val updateJson =
          Json.obj("expectedVersion" -> Some(f.container.version), "containerTypeId" -> containerType.id)
        val reply = makeAuthRequest(POST, uri("containerType", f.container.id.id), updateJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError: container and containerTypes not compatible")
      }

      describe("fail when updating container type ID with invalid version") {
        updateWithInvalidVersionSharedBehaviour { container =>
          (uri("containerType", container.id.id),
           Json.obj("containerTypeId" -> nameGenerator.next[Container]))
        }
      }
    }

    describe("DELETE /api/centres/containers/:id/:version") {

      it("remove a container") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        val reply = makeAuthRequest(DELETE, uri(f.container.id.id, f.container.version.toString)).value
        reply must beOkResponseWithJsonReply

        val result = (contentAsJson(reply) \ "data").validate[Boolean]
        result must be(jsSuccess)
        result.get must be(true)
        containerRepository.getByKey(f.container.id) mustFail ("IdNotFound: container")
      }

      it("fail when removing a container that does not exist") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        val reply = makeAuthRequest(DELETE, uri(f.container.id.id, f.container.version.toString)).value
        reply must beNotFoundWithMessage("IdNotFound: container id")
      }

      it("fail when container is being used by another container or specimen") {
        val f = fixture
        f.allEntities.foreach(addToRepository)

        f.container match {
          case _: RootContainer | _: StorageContainer =>
            f.createChild(factory).map {
              case (childContainer, childContainerType, childSchema) =>
                Set(childContainer, childContainerType, childSchema).foreach(addToRepository)
            }
          case c: SpecimenContainer =>
            val specimen = factory.createUsableSpecimen().copy(containerId = Some(f.container.id))
            specimenRepository.put(specimen)
        }
        val reply = makeAuthRequest(DELETE, uri(f.container.id.id, f.container.version.toString)).value
        reply must beBadRequestWithMessage("EntityInUse: container in use")
      }

    }
  }

  def validateContainer(container: Container, json: JsValue): Unit = {
    val newContainerId = (json \ "data" \ "id").validate[ContainerId]
    newContainerId must be(jsSuccess)
    val newId = newContainerId.get

    val (jsonContainer, updatedContainer, repoContainer) = container match {
      case c: RootContainer =>
        ((json \ "data").validate[RootContainerDto],
         c.copy(id = newId),
         containerRepository.getRootContainer(newId))
      case c: StorageContainer =>
        ((json \ "data").validate[StorageContainerDto],
         c.copy(id = newId),
         containerRepository.getStorageContainer(newId))
      case c: SpecimenContainer =>
        ((json \ "data").validate[SpecimenContainerDto],
         c.copy(id = newId),
         containerRepository.getSpecimenContainer(newId))
    }
    jsonContainer must be(jsSuccess)
    jsonContainer.get must matchDtoToContainer(updatedContainer)
    repoContainer mustSucceed { repoContainer =>
      repoContainer must matchContainer(updatedContainer)
    }
  }

  protected def updateWithInvalidVersionSharedBehaviour(func: C => (Url, JsObject)) =
    it("should return bad request") {
      val f = fixture
      f.allEntities.foreach(addToRepository)
      val (url, json) = func(f.container)

      val reqJson = Json.obj("expectedVersion" -> Some(f.containerType.version + 1)) ++ json
      val reply   = makeAuthRequest(POST, url, reqJson)
      reply.value must beBadRequestWithMessage("expected version doesn't match current version")
    }

  protected def constraintsToJson(constraints: ContainerConstraints): JsObject =
    Json.obj("name"              -> constraints.name,
             "description"       -> constraints.description,
             "anatomicalSources" -> constraints.anatomicalSources,
             "preservationTypes" -> constraints.preservationTypes,
             "specimenTypes"     -> constraints.specimenTypes)

  protected def listSingle(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, C)
    ) =
    it("list single container") {
      val (url, expectedContainer) = setupFunc()
      val reply                    = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beSingleItemResults(offset, maybeNext, maybePrev)

      val replyContainers = (json \ "data" \ "items").validate[List[ContainerDto]]
      replyContainers must be(jsSuccess)
      replyContainers.get.foreach { _ must matchDtoToContainer(expectedContainer) }
    }

  protected def listMultiple(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, List[Container])
    ) =
    it("list multiple types") {
      val (url, expectedContainers) = setupFunc()

      val reply = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beMultipleItemResults(offset    = offset,
                                      total     = expectedContainers.size.toLong,
                                      maybeNext = maybeNext,
                                      maybePrev = maybePrev)

      val replyContainers = (json \ "data" \ "items").validate[List[ContainerDto]]
      replyContainers must be(jsSuccess)

      (replyContainers.get zip expectedContainers).foreach {
        case (replyContainer, expectedContainer) =>
          replyContainer must matchDtoToContainer(expectedContainer)
      }
    }

}
