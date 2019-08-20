package org.biobank.controllers.centres

//import java.time.OffsetDateTime
import org.biobank.controllers.PagedResultsSharedSpec
import org.biobank.domain._
import org.biobank.domain.PreservationTemperature._
import org.biobank.domain.containers._
import org.biobank.domain.centres._
//import org.biobank.domain.studies.{Study, StudyId}
import org.biobank.dto._
import org.biobank.fixtures.{ControllerFixture} //, Url}
import org.biobank.matchers.PagedResultsMatchers
//import org.scalatest.prop.TableDrivenPropertyChecks._
import play.api.libs.json._
import play.api.test.Helpers._

/**
  * Tests the REST API for [[Container]]s.
  */
class ContainersControllerSpec
    extends ControllerFixture
    with PagedResultsSharedSpec
    with PagedResultsMatchers {

  import org.biobank.TestUtils._
  import org.biobank.matchers.JsonMatchers._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._

  protected val basePath = "centres/containers"

  //private def uri(container: Container): Url = uri(container.id.id)

  //private def uri(container: Container, path: String): Url = uri(path, container.id.id)

  describe("Centre REST API") {

    describe("POST /api/containers") {

      it("add a root container") {
        val (container, containerType, centre) = createEntities
        centreRepository.put(centre)
        containerTypeRepository.put(containerType)
        val addJson = containerToAddRootJson(container, containerType, centre.locations.toSeq(0))
        val reply = makeAuthRequest(POST, uri(""), addJson).value
        reply must beOkResponseWithJsonReply

        val json = contentAsJson(reply)
        val replyContainer = (json \ "data").validate[StorageContainerDto]
        replyContainer must be (jsSuccess)

        val newContainerId = ContainerId(replyContainer.get.id)
        val updatedContainer = container.copy(id = newContainerId)

        replyContainer.get must matchDtoToStorageContainer(updatedContainer)
        containerRepository.getStorageContainer(newContainerId) mustSucceed { repoContainer =>
          repoContainer must matchContainer(updatedContainer)
        }
      }

      it("adding a container fails if centre is not defined") {
        val (container, containerType, centre) = createEntities
        val addJson = containerToAddRootJson(container, containerType, centre.locations.toSeq(0))
        val reply = makeAuthRequest(POST, uri(""), addJson).value
        reply must beNotFoundWithMessage("IdNotFound: centre id")
      }

      it("adding a container fails if container type is not defined") {
        val (container, containerType, centre) = createEntities
        centreRepository.put(centre)
        val addJson = containerToAddRootJson(container, containerType, centre.locations.toSeq(0))
        val reply = makeAuthRequest(POST, uri(""), addJson).value
        reply must beNotFoundWithMessage("IdNotFound: container type id")
      }

    }

  }

  private def createEntities(): (StorageContainer, StorageContainerType, EnabledCentre) = {
    val location = factory.createLocation
    val centre = factory.defaultEnabledCentre.copy(locations = Set(location))
    val containerType = factory.defaultStorageContainerType
    val sharedProperties = ContainerSharedProperties(centre.id,
                                                     location.id,
                                                     PreservationTemperature.Minus80celcius)
    val rootContainer = factory.createStorageContainer.copy(sharedProperties = Some(sharedProperties))
    (rootContainer, containerType, centre)
  }

  private def containerToAddRootJson(container: Container,
                                     containerType: ContainerType,
                                     location: Location): JsValue = {
    container.parentId mustBe None
    Json.obj("inventoryId"     -> container.inventoryId,
             "label"           -> container.label,
             "centreId"        -> containerType.centreId,
             "locationId"      -> location.id,
             "temperature"     -> container.sharedProperties.value.temperature,
             "containerTypeId" -> containerType.id)
  }

}
