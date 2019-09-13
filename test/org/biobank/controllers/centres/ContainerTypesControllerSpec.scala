package org.biobank.controllers.centres

import org.biobank.controllers.PagedResultsSharedSpec
import org.biobank.domain.centres.{CentreId, EnabledCentre}
import org.biobank.domain.containers._
import org.biobank.dto._
import org.biobank.fixtures.{ControllerFixture, Url}
import org.biobank.matchers.PagedResultsMatchers
import play.api.libs.json._
import play.api.test.Helpers._

case class ContainerTypeFixture[T <: ContainerType](
    centre:         EnabledCentre,
    schema:         ContainerSchema,
    containerTypes: List[T],
    containerType:  T)

class StorageContainerTypesControllerSpec extends ContainerTypesControllerSpec[StorageContainerType] {

  protected def addUri() = uri("storage")

  protected def createContainer(containerType: StorageContainerType, schema: ContainerSchema): Container = {
    val labelInfo = ContainerSchemaLabel(schema.id, schema.labels.headOption.value)
    factory.createStorageContainer(containerType, factory.defaultRootContainer, labelInfo)
  }

  protected def fixture(numContainerTypes: Int = 1, shared: Boolean = false) = {
    val location = factory.createLocation
    val centre   = factory.createEnabledCentre.copy(locations = Set(location))

    val schema = factory.createContainerSchema
      .copy(shared = false, labels = Set(factory.createContainerSchemaLabel.label))
    val containerTypes = (1 to numContainerTypes).map { _ =>
      factory.createStorageContainerType.copy(shared = shared)
    }.toList
    val containerType = containerTypes.headOption.value

    ContainerTypeFixture(centre, schema, containerTypes, containerType)
  }

  protected def fixtureAddAllButCtype(
      numContainerTypes: Int     = 1,
      shared:            Boolean = false
    ): ContainerTypeFixture[StorageContainerType] = {
    val f = fixture(numContainerTypes, shared)
    Set(f.centre, f.schema).foreach(addToRepository)
    f
  }

  protected def fixtureAddAll(
      numContainerTypes: Int     = 1,
      shared:            Boolean = false
    ): ContainerTypeFixture[StorageContainerType] = {
    val f = fixture(numContainerTypes, shared)
    (List(f.centre, f.schema) ++ f.containerTypes).foreach(addToRepository)
    f
  }
}

class SpecimenContainerTypesControllerSpec extends ContainerTypesControllerSpec[SpecimenContainerType] {

  protected def addUri() = uri("specimen")

  protected def createContainer(containerType: SpecimenContainerType, schema: ContainerSchema): Container = {
    val labelInfo = ContainerSchemaLabel(schema.id, schema.labels.headOption.value)
    factory.createSpecimenContainer(containerType, factory.defaultStorageContainer, labelInfo)
  }

  protected def fixture(numContainerTypes: Int = 1, shared: Boolean = false) = {
    val location = factory.createLocation
    val centre   = factory.createEnabledCentre.copy(locations = Set(location))

    val schema = factory.createContainerSchema
      .copy(shared = false, labels = Set(factory.createContainerSchemaLabel.label))
    val containerTypes = (1 to numContainerTypes).map { _ =>
      factory.createSpecimenContainerType.copy(shared = shared)
    }.toList
    val containerType = containerTypes.headOption.value

    ContainerTypeFixture(centre, schema, containerTypes, containerType)
  }

  protected def fixtureAddAllButCtype(
      numContainerTypes: Int     = 1,
      shared:            Boolean = false
    ): ContainerTypeFixture[SpecimenContainerType] = {
    val f = fixture(numContainerTypes)
    Set(f.centre, f.schema).foreach(addToRepository)
    f
  }

  protected def fixtureAddAll(
      numContainerTypes: Int     = 1,
      shared:            Boolean = false
    ): ContainerTypeFixture[SpecimenContainerType] = {
    val f = fixture(numContainerTypes)
    (List(f.centre, f.schema) ++ f.containerTypes).foreach(addToRepository)
    f
  }
}

/**
 * Tests the REST API for [[Container]]s.
 */
trait ContainerTypesControllerSpec[T <: ContainerType]
    extends ControllerFixture with PagedResultsSharedSpec with PagedResultsMatchers {

  import org.biobank.TestUtils._
  import org.biobank.matchers.JsonMatchers._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._

  protected val basePath = "centres/containers/types"

  describe("Container Types REST API") {

    describe("GET /api/centres/containers/types/:slug") {

      it("get a single container type by slug") {
        val f     = fixtureAddAll()
        val reply = makeAuthRequest(GET, uri(f.containerType.slug.id)).value
        reply must beOkResponseWithJsonReply

        val json = contentAsJson(reply)
        validateContainerType(f.containerType, (json \ "data") get)
      }

      it("fail when querying for a single type and slug is invalid") {
        val f     = fixture()
        val reply = makeAuthRequest(GET, uri(f.containerType.slug.id)).value
        reply must beNotFoundWithMessage("EntityCriteriaNotFound: container type slug")
      }
    }

    describe("GET /api/centres/containers/types/search/:centreId") {

      it("list none") {
        val f = fixtureAddAllButCtype()
        uri("search", f.centre.id.id) must beEmptyResults
      }

      describe("list a single container type") {
        listSingleType() { () =>
          val f = fixtureAddAll()
          (uri("search", f.centre.id.id), f.containerType)
        }
      }

      describe("get all types for a centre") {
        listMultipleTypes() { () =>
          val f = fixtureAddAll(2)
          (uri("search", f.centre.id.id), f.containerTypes)
        }
      }

      describe("list types sorted by name") {

        describe("in ascending order") {
          listMultipleTypes() { () =>
            val f = fixtureAddAll(2)
            (uri("search", f.centre.id.id).addQueryString("sort=name"),
             f.containerTypes.sortWith(_.name < _.name))
          }
        }

        describe("in descending order") {
          listMultipleTypes() { () =>
            val f = fixtureAddAll(2)
            (uri("search", f.centre.id.id).addQueryString("sort=-name"),
             f.containerTypes.sortWith(_.name > _.name))
          }
        }
      }

    }

    describe("POST /api/containers/types") {

      val url = addUri()

      it("add a container type") {
        val f       = fixtureAddAllButCtype()
        val addJson = containerTypeToAddJson(f.containerType)
        val reply   = makeAuthRequest(POST, url, addJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        validateContainerType(f.containerType, (json \ "data") get)
      }

      it("adding a container type fails if centre is not defined") {
        val f       = fixture()
        val addJson = containerTypeToAddJson(f.containerType)
        val reply   = makeAuthRequest(POST, url, addJson).value
        reply must beNotFoundWithMessage("IdNotFound: centre id")
      }

      describe("duplicate names") {
        it("when in the same centre, fail when adding a type with a duplicate name") {
          val f       = fixtureAddAll()
          val addJson = containerTypeToAddJson(f.containerType)

          val reply = makeAuthRequest(POST, url, addJson).value
          reply must beForbiddenRequestWithMessage(
            "EntityCriteriaError: container type with name already exists"
          )
        }

        it("when NOT for the same centre and not shared, container types allowed to have a duplicate name") {
          val containerTypes = (1 to 2).map { _ =>
            val f = fixtureAddAllButCtype(shared = false)
            f.containerType
          }
          addToRepository(containerTypes(0))
          val duplicateName = containerTypes(0).name
          val containerType = containerTypes(1).withName(duplicateName).toOption.value
          val addJson       = containerTypeToAddJson(containerType)
          val reply         = makeAuthRequest(POST, url, addJson).value
          reply must beOkResponseWithJsonReply
        }
      }
    }

    describe("POST /api/containers/types/name/:id") {

      it("update a schema's name") {
        val newName    = nameGenerator.next[ContainerSchema]
        val f          = fixtureAddAll()
        val updateJson = Json.obj("expectedVersion" -> Some(f.containerType.version), "name" -> newName)
        val reply      = makeAuthRequest(POST, uri("name", f.containerType.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json         = contentAsJson(reply)
        val updatedCtype = f.containerType.withName(newName).toOption.value

        validateContainerType(updatedCtype, (json \ "data") get)
      }

      describe("duplicate names") {
        it("when in same centre, not update a centre with a duplicate name") {
          val f             = fixtureAddAll(2)
          val duplicateName = f.containerTypes(0).name
          val updateJson =
            Json.obj("expectedVersion" -> Some(f.containerTypes(1).version), "name" -> duplicateName)
          val reply = makeAuthRequest(POST, uri("name", f.containerTypes(1).id.id), updateJson).value
          reply must beForbiddenRequestWithMessage(
            "EntityCriteriaError: container type with name already exists"
          )
        }

        it("when NOT for the same centre and not shared, container types allowed to have a duplicate name") {
          val containerTypes = (1 to 2).map { _ =>
            val f = fixtureAddAll(shared = false)
            f.containerType
          }
          val duplicateName = containerTypes(0).name
          val updateJson =
            Json.obj("expectedVersion" -> Some(containerTypes(1).version), "name" -> duplicateName)
          val reply = makeAuthRequest(POST, uri("name", containerTypes(1).id.id), updateJson).value
          reply must beOkResponseWithJsonReply
        }

      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { ctype =>
          (uri("name", ctype.id.id), Json.obj("name" -> nameGenerator.next[ContainerType]))
        }
      }
    }

    describe("POST /api/containers/types/description/:id") {

      it("update a container type's description") {
        val newDescription = nameGenerator.next[ContainerType]
        val f              = fixtureAddAll()
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.containerType.version), "description" -> newDescription)
        val reply = makeAuthRequest(POST, uri("description", f.containerType.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedType = f.containerType.withDescription(Some(newDescription)).toOption.value
        validateContainerType(updatedType, (json \ "data") get)
      }

      it("clear a container type's description") {
        val f          = fixtureAddAll()
        val updateJson = Json.obj("expectedVersion" -> Some(f.containerType.version))
        val reply      = makeAuthRequest(POST, uri("description", f.containerType.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedType = f.containerType.withDescription(None).toOption.value
        validateContainerType(updatedType, (json \ "data") get)
      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { ctype =>
          (uri("description", ctype.id.id), Json.obj("description" -> nameGenerator.next[ContainerType]))
        }
      }
    }

  }

  describe("POST /api/containers/type/centre/:id") {

    it("update the centre a container type belongs to") {
      val f      = fixtureAddAll()
      val centre = factory.createEnabledCentre()
      Set(centre).foreach(addToRepository)

      val updateJson =
        Json.obj("expectedVersion" -> Some(f.containerType.version), "centreId" -> centre.id.id)
      val reply = makeAuthRequest(POST, uri("centre", f.containerType.id.id), updateJson).value
      reply must beOkResponseWithJsonReply
      val json = contentAsJson(reply)

      val updatedType = f.containerType.withCentre(centre.id).toOption.value
      validateContainerType(updatedType, (json \ "data") get)
    }

    it("cannot be updated to a centre that does not exist") {
      val f      = fixtureAddAll()
      val centre = factory.createEnabledCentre()

      val updateJson =
        Json.obj("expectedVersion" -> Some(f.containerType.version), "centreId" -> centre.id.id)
      val reply = makeAuthRequest(POST, uri("centre", f.containerType.id.id), updateJson).value
      reply must beNotFoundWithMessage("IdNotFound: centre id")
    }

    describe("fail when updating name with invalid version") {
      updateWithInvalidVersionSharedBehaviour { ctype =>
        (uri("centre", ctype.id.id), Json.obj("centreId" -> CentreId(nameGenerator.next[ContainerType])))
      }
    }
  }

  describe("POST /api/containers/type/schema/:id") {

    it("update the schema a container type belongs to") {
      val f      = fixtureAddAll()
      val schema = factory.createContainerSchema()
      addToRepository(schema)

      val updateJson =
        Json.obj("expectedVersion" -> Some(f.containerType.version), "schemaId" -> schema.id.id)
      val reply = makeAuthRequest(POST, uri("schema", f.containerType.id.id), updateJson).value
      reply must beOkResponseWithJsonReply
      val json = contentAsJson(reply)

      val updatedType = f.containerType.withSchema(schema.id).toOption.value
      validateContainerType(updatedType, (json \ "data") get)
    }

    it("cannot be updated to a schema that does not exist") {
      val f      = fixtureAddAll()
      val schema = factory.createContainerSchema()

      val updateJson =
        Json.obj("expectedVersion" -> Some(f.containerType.version), "schemaId" -> schema.id.id)
      val reply = makeAuthRequest(POST, uri("schema", f.containerType.id.id), updateJson).value
      reply must beNotFoundWithMessage("IdNotFound: container schema id")
    }

    describe("fail when updating name with invalid version") {
      updateWithInvalidVersionSharedBehaviour { ctype =>
        (uri("schema", ctype.id.id),
         Json.obj("schemaId" -> ContainerSchemaId(nameGenerator.next[ContainerType])))
      }
    }
  }

  describe("POST /api/containers/types/shared/:id") {

    it("update a type's shared property") {
      val f = fixtureAddAll()

      Set(true, false).foreach { shared =>
        val containerType = containerTypeRepository.getByKey(f.containerType.id).toOption.value
        val updateJson =
          Json.obj("expectedVersion" -> Some(containerType.version), "shared" -> shared)
        val reply = makeAuthRequest(POST, uri("shared", containerType.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedType = containerType.withShared(shared).toOption.value
        validateContainerType(updatedType, (json \ "data") get)
      }
    }

    describe("fail when updating name with invalid version") {
      updateWithInvalidVersionSharedBehaviour { ctype =>
        (uri("shared", ctype.id.id), Json.obj("shared" -> false))
      }
    }
  }

  describe("POST /api/containers/types/enabled/:id") {

    it("update a type's enabled property") {
      val f = fixtureAddAll()

      Set(true, false).foreach { enabled =>
        val containerType = containerTypeRepository.getByKey(f.containerType.id).toOption.value
        val updateJson =
          Json.obj("expectedVersion" -> Some(containerType.version), "enabled" -> enabled)
        val reply = makeAuthRequest(POST, uri("enabled", containerType.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedType = containerType.withEnabled(enabled).toOption.value
        validateContainerType(updatedType, (json \ "data") get)
      }
    }

    describe("fail when updating name with invalid version") {
      updateWithInvalidVersionSharedBehaviour { ctype =>
        (uri("enabled", ctype.id.id), Json.obj("enabled" -> false))
      }
    }
  }

  describe("DELETE /api/containers/type/:id/:version") {

    it("remove a type") {
      val f     = fixtureAddAll()
      val reply = makeAuthRequest(DELETE, uri(f.containerType.id.id, f.containerType.version.toString)).value
      reply must beOkResponseWithJsonReply

      val result = (contentAsJson(reply) \ "data").validate[Boolean]
      result must be(jsSuccess)
      result.get must be(true)
      containerTypeRepository.getByKey(f.containerType.id) mustFail ("IdNotFound: container type")
    }

    it("fail when type does not exist") {
      val f     = fixture()
      val reply = makeAuthRequest(DELETE, uri(f.containerType.id.id, f.containerType.version.toString)).value
      reply must beNotFoundWithMessage("IdNotFound: container type id")
    }

    it("fail when container type is being used by a container") {
      val f         = fixtureAddAll()
      val container = createContainer(f.containerType, f.schema)
      addToRepository(container)
      val reply = makeAuthRequest(DELETE, uri(f.containerType.id.id, f.containerType.version.toString)).value
      reply must beBadRequestWithMessage("EntityInUse: container type in use")
    }

  }

  protected def addUri(): Url

  protected def createContainer(containerType: T, schema: ContainerSchema): Container

  protected def fixture(numContainerTypes: Int = 1, shared: Boolean = false): ContainerTypeFixture[T]

  protected def fixtureAddAllButCtype(
      numContainerTypes: Int     = 1,
      shared:            Boolean = false
    ): ContainerTypeFixture[T]

  protected def fixtureAddAll(numContainerTypes: Int = 1, shared: Boolean = false): ContainerTypeFixture[T]

  private def containerTypeToAddJson(containerType: ContainerType): JsValue =
    Json.obj("name"        -> containerType.name,
             "description" -> containerType.description,
             "centreId"    -> containerType.centreId.id,
             "shared"      -> containerType.shared,
             "schemaId"    -> containerType.schemaId)

  def validateContainerType(containerType: ContainerType, json: JsValue): Unit = {
    val containerTypeFromJson = json.validate[ContainerTypeDto]
    containerTypeFromJson must be(jsSuccess)

    val newContainerTypeId = ContainerTypeId(containerTypeFromJson.get.id)
    val updatedContainerType = containerType match {
      case ct: StorageContainerType  => ct.copy(id = newContainerTypeId)
      case ct: SpecimenContainerType => ct.copy(id = newContainerTypeId)
    }

    containerTypeFromJson.get must matchDtoToContainerType(updatedContainerType)
    containerTypeRepository.getByKey(newContainerTypeId) mustSucceed { repoContainerType =>
      repoContainerType must matchContainerType(updatedContainerType)
    }
  }

  private def updateWithInvalidVersionSharedBehaviour(func: ContainerType => (Url, JsObject)) =
    it("should return bad request") {
      val f           = fixtureAddAll()
      val (url, json) = func(f.containerType)

      val reqJson = Json.obj("expectedVersion" -> Some(f.containerType.version + 1)) ++ json
      val reply   = makeAuthRequest(POST, url, reqJson)
      reply.value must beBadRequestWithMessage("expected version doesn't match current version")
    }

  private def listSingleType(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, ContainerType)
    ) =
    it("list single type") {
      val (url, expectedType) = setupFunc()
      val reply               = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beSingleItemResults(offset, maybeNext, maybePrev)

      val replyContainerTypes = (json \ "data" \ "items").validate[List[ContainerTypeDto]]
      replyContainerTypes must be(jsSuccess)
      replyContainerTypes.get.foreach { _ must matchDtoToContainerType(expectedType) }
    }

  private def listMultipleTypes(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, List[ContainerType])
    ) =
    it("list multiple types") {
      val (url, expectedTypes) = setupFunc()

      val reply = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beMultipleItemResults(offset    = offset,
                                      total     = expectedTypes.size.toLong,
                                      maybeNext = maybeNext,
                                      maybePrev = maybePrev)

      val replyContainerTypes = (json \ "data" \ "items").validate[List[ContainerTypeDto]]
      replyContainerTypes must be(jsSuccess)

      (replyContainerTypes.get.sortWith(_.id < _.id) zip expectedTypes.sortWith(_.id.id < _.id.id)).foreach {
        case (replyType, expectedType) =>
          replyType must matchDtoToContainerType(expectedType)
      }
    }
}
