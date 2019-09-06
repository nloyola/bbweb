package org.biobank.controllers.centres

import java.time.OffsetDateTime
import org.biobank.controllers.PagedResultsSharedSpec
import org.biobank.domain.Slug
import org.biobank.domain.centres.CentreId
import org.biobank.domain.containers._
import org.biobank.dto._
import org.biobank.fixtures.{ControllerFixture, Url}
import org.biobank.matchers.PagedResultsMatchers
import play.api.libs.json._
import play.api.test.Helpers._

/**
 * Tests the REST API for [[Container]]s.
 */
class ContainerSchemasControllerSpec
    extends ControllerFixture with PagedResultsSharedSpec with PagedResultsMatchers {

  import org.biobank.TestUtils._
  import org.biobank.matchers.JsonMatchers._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._

  protected val basePath = "centres/containers/schemas"

  describe("Container Schemas REST API") {

    describe("GET /api/centres/containers/schemas/:slug") {

      it("get a single container schema by slug") {
        val f     = fixtureAddAll
        val reply = makeAuthRequest(GET, uri(f.schema.slug.id)).value
        reply must beOkResponseWithJsonReply

        val json = contentAsJson(reply)
        validateSchema(f.schema, (json \ "data") get)
      }

      it("fail when querying for a single schema and slug is invalid") {
        val f     = new SchemaFixture
        val reply = makeAuthRequest(GET, uri(f.schema.slug.id)).value
        reply must beNotFoundWithMessage("EntityCriteriaNotFound: container schema slug")
      }
    }

    describe("GET /api/centres/containers/schemas/search/:centreId") {

      it("list none") {
        val f = fixtureAddCentre
        uri("search", f.centre.id.id) must beEmptyResults
      }

      describe("list a single container schema") {
        listSingleSchema() { () =>
          val f = fixtureAddCentre
          Set(f.schema).foreach(addToRepository)
          (uri("search", f.centre.id.id), f.schema)
        }
      }

      describe("get all schemas for a centre") {
        listMultipleSchemas() { () =>
          val centre = factory.createEnabledCentre
          val schemas = (0 until 2).map { _ =>
            factory.createContainerSchema
          }.toList

          (List(centre) ++ schemas).foreach(addToRepository)

          (uri("search", centre.id.id), schemas)
        }
      }

      describe("list schemas sorted by name") {
        def commonSetup = {
          val centre = factory.createEnabledCentre
          val schemas = (1 to 4).map { index =>
            factory.createContainerSchema.copy(name = s"CS$index")
          }.toList
          (List(centre) ++ schemas).foreach(addToRepository)
          (centre, schemas)
        }

        describe("in ascending order") {
          listMultipleSchemas() { () =>
            val (centre, schemas) = commonSetup
            (uri("search", centre.id.id).addQueryString("sort=name"), schemas.sortWith(_.name < _.name))
          }
        }

        describe("in descending order") {
          listMultipleSchemas() { () =>
            val (centre, schemas) = commonSetup
            (uri("search", centre.id.id).addQueryString("sort=-name"), schemas.sortWith(_.name > _.name))
          }
        }
      }

    }

    describe("POST /api/containers/schemas") {

      val url = uri("")

      it("add a schema") {
        val f       = fixtureAddCentre
        val schema  = f.schema.copy(labels = Set.empty[String])
        val addJson = schemaToAddJson(schema)
        val reply   = makeAuthRequest(POST, url, addJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        validateSchema(schema, (json \ "data") get)
      }

      it("adding a schema fails if centre is not defined") {
        val f       = new SchemaFixture
        val addJson = schemaToAddJson(f.schema)
        val reply   = makeAuthRequest(POST, url, addJson).value
        reply must beNotFoundWithMessage("IdNotFound: centre id")
      }

      it("fail when adding a schema with a duplicate name") {
        val f       = fixtureAddAll
        val addJson = schemaToAddJson(f.schema)

        val reply = makeAuthRequest(POST, uri(""), addJson).value
        reply must beForbiddenRequestWithMessage(
          "EntityCriteriaError: container schema with name already exists"
        )
      }
    }

    describe("POST /api/containers/schemas/name/:id") {

      it("update a schema's name") {
        val newName    = nameGenerator.next[ContainerSchema]
        val f          = fixtureAddAll
        val updateJson = Json.obj("expectedVersion" -> Some(f.schema.version), "name" -> newName)
        val reply      = makeAuthRequest(POST, uri("name", f.schema.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedSchema = f.schema.copy(version = f.schema.version + 1,
                                          name         = newName,
                                          slug         = Slug(newName),
                                          timeModified = Some(OffsetDateTime.now))

        validateSchema(updatedSchema, (json \ "data") get)
      }

      describe("duplicate names") {
        it("when all schemas are in same centres, not update a centre with a duplicate name") {
          val centre = factory.createEnabledCentre
          val schemas = (1 to 2).map { _ =>
            factory.createContainerSchema
          }
          (List(centre) ++ schemas).foreach(addToRepository)

          val duplicateName = schemas(0).name
          val updateJson =
            Json.obj("expectedVersion" -> Some(schemas(1).version), "name" -> duplicateName)
          val reply = makeAuthRequest(POST, uri("name", schemas(1).id.id), updateJson).value
          reply must beForbiddenRequestWithMessage(
            "EntityCriteriaError: container schema with name already exists"
          )
        }

        it(
          "when schemas are NOT for the same centre and not shared, schemas allowed to have a duplicate name"
        ) {
          val schemas = (1 to 2).map { _ =>
            val centre = factory.createEnabledCentre()
            val schema = factory.createContainerSchema.copy(shared = false, centreId = centre.id)
            Set(centre, schema).foreach(addToRepository)
            schema
          }

          val duplicateName = schemas(0).name
          val updateJson =
            Json.obj("expectedVersion" -> Some(schemas(1).version), "name" -> duplicateName)
          val reply = makeAuthRequest(POST, uri("name", schemas(1).id.id), updateJson).value
          reply must beOkResponseWithJsonReply
        }

      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { schema =>
          (uri("name", schema.id.id), Json.obj("name" -> nameGenerator.next[ContainerSchema]))
        }
      }
    }

    describe("POST /api/containers/schemas/description/:id") {

      it("update a schema's description") {
        val newDescription = nameGenerator.next[ContainerSchema]
        val f              = fixtureAddAll
        val updateJson =
          Json.obj("expectedVersion" -> Some(f.schema.version), "description" -> newDescription)
        val reply = makeAuthRequest(POST, uri("description", f.schema.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedSchema = f.schema.copy(version = f.schema.version + 1,
                                          description  = Some(newDescription),
                                          timeModified = Some(OffsetDateTime.now))

        validateSchema(updatedSchema, (json \ "data") get)
      }

      it("clear a schema's description") {
        val f          = fixtureAddAll
        val updateJson = Json.obj("expectedVersion" -> Some(f.schema.version))
        val reply      = makeAuthRequest(POST, uri("description", f.schema.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedSchema = f.schema.copy(version = f.schema.version + 1,
                                          description  = None,
                                          timeModified = Some(OffsetDateTime.now))

        validateSchema(updatedSchema, (json \ "data") get)
      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { schema =>
          (uri("description", schema.id.id), Json.obj("description" -> nameGenerator.next[ContainerSchema]))
        }
      }
    }

    describe("POST /api/containers/schemas/shared/:id") {

      it("update a schema's shared property") {
        val f = fixtureAddAll

        Set(true, false).zipWithIndex.foreach {
          case (shared, index) =>
            val updateJson =
              Json.obj("expectedVersion" -> Some(f.schema.version + index), "shared" -> shared)
            val reply = makeAuthRequest(POST, uri("shared", f.schema.id.id), updateJson).value
            reply must beOkResponseWithJsonReply
            val json = contentAsJson(reply)

            val updatedSchema = f.schema.copy(version = f.schema.version + 1 + index,
                                              shared       = shared,
                                              timeModified = Some(OffsetDateTime.now))

            validateSchema(updatedSchema, (json \ "data") get)
        }
      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { schema =>
          (uri("shared", schema.id.id), Json.obj("shared" -> false))
        }
      }
    }

    describe("POST /api/containers/schema/centre/:id") {

      it("update the centre a schema belongs to") {
        val f      = fixtureAddAll
        val centre = factory.createEnabledCentre()
        Set(centre).foreach(addToRepository)

        val updateJson = Json.obj("expectedVersion" -> Some(f.schema.version), "centreId" -> centre.id.id)
        val reply      = makeAuthRequest(POST, uri("centre", f.schema.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedSchema = f.schema.copy(version = f.schema.version + 1,
                                          centreId     = centre.id,
                                          timeModified = Some(OffsetDateTime.now))

        validateSchema(updatedSchema, (json \ "data") get)
      }

      it("cannot be updated to a centre that does not exist") {
        val f      = fixtureAddAll
        val centre = factory.createEnabledCentre()

        val updateJson = Json.obj("expectedVersion" -> Some(f.schema.version), "centreId" -> centre.id.id)
        val reply      = makeAuthRequest(POST, uri("centre", f.schema.id.id), updateJson).value
        reply must beNotFoundWithMessage("IdNotFound: centre id")
      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { schema =>
          (uri("centre", schema.id.id), Json.obj("centreId" -> CentreId(nameGenerator.next[ContainerSchema])))
        }
      }
    }

    describe("POST /api/containers/schema/labels/:id") {

      it("update the labels in a schema") {
        val f            = fixtureAddAll
        val schemaLabels = Set(factory.createContainerSchemaLabel)
        val labels       = schemaLabels.map(_.label)

        val updateJson = Json.obj("expectedVersion" -> Some(f.schema.version), "labels" -> labels)
        val reply      = makeAuthRequest(POST, uri("labels", f.schema.id.id), updateJson).value
        reply must beOkResponseWithJsonReply
        val json = contentAsJson(reply)

        val updatedSchema = f.schema.copy(version = f.schema.version + 1,
                                          labels       = labels,
                                          timeModified = Some(OffsetDateTime.now))

        validateSchema(updatedSchema, (json \ "data") get)
      }

      it("labels cannot be updated on schema not in the system") {
        val f          = new SchemaFixture
        val updateJson = Json.obj("expectedVersion" -> Some(f.schema.version), "labels" -> List.empty[String])
        val reply      = makeAuthRequest(POST, uri("labels", f.schema.id.id), updateJson).value
        reply must beNotFoundWithMessage("IdNotFound: container schema id")
      }

      describe("fail when updating name with invalid version") {
        updateWithInvalidVersionSharedBehaviour { schema =>
          (uri("labels", schema.id.id), Json.obj("labels" -> List.empty[String]))
        }
      }
    }

    describe("DELETE /api/containers/schema/:id/:version") {

      it("remove a schema") {
        val f     = fixtureAddAll
        val reply = makeAuthRequest(DELETE, uri(f.schema.id.id, f.schema.version.toString)).value
        reply must beOkResponseWithJsonReply

        val result = (contentAsJson(reply) \ "data").validate[Boolean]
        result must be(jsSuccess)
        result.get must be(true)
        containerSchemaRepository.getByKey(f.schema.id) mustFail ("IdNotFound: container schema")
      }

      it("fail when schema does not exist") {
        val f     = new SchemaFixture
        val reply = makeAuthRequest(DELETE, uri(f.schema.id.id, f.schema.version.toString)).value
        reply must beNotFoundWithMessage("IdNotFound: container schema id")
      }

      it("fail when schema is being used by a container type") {
        val f     = fixtureAddAll
        val ctype = factory.createStorageContainerType.copy(schemaId = f.schema.id)
        addToRepository(ctype)
        val reply = makeAuthRequest(DELETE, uri(f.schema.id.id, f.schema.version.toString)).value
        reply must beBadRequestWithMessage("EntityInUse: schema in use")
      }

    }

  }

  private class SchemaFixture {
    val centre = factory.defaultEnabledCentre

    val schema = factory.createContainerSchema
      .copy(shared = false, labels = Set(factory.createContainerSchemaLabel.label))
  }

  private def fixtureAddCentre(): SchemaFixture = {
    val f = new SchemaFixture
    addToRepository(f.centre)
    f
  }

  private def fixtureAddAll(): SchemaFixture = {
    val f = new SchemaFixture
    Set(f.centre, f.schema).foreach(addToRepository)
    f
  }

  private def schemaToAddJson(schema: ContainerSchema): JsValue =
    Json.obj("name"        -> schema.name,
             "description" -> schema.description,
             "shared"      -> schema.shared,
             "centreId"    -> schema.centreId.id,
             "labels"      -> schema.labels)

  def validateSchema(schema: ContainerSchema, json: JsValue): Unit = {
    val schemaFromJson = json.validate[ContainerSchemaDto]
    schemaFromJson must be(jsSuccess)

    val newSchemaId   = ContainerSchemaId(schemaFromJson.get.id)
    val updatedSchema = schema.copy(id = newSchemaId)

    schemaFromJson.get must matchDtoToContainerSchema(updatedSchema)
    containerSchemaRepository.getByKey(newSchemaId) mustSucceed { repoSchema =>
      repoSchema must matchContainerSchema(updatedSchema)
    }
  }

  private def updateWithInvalidVersionSharedBehaviour(func: ContainerSchema => (Url, JsObject)) =
    it("should return bad request") {
      val f = new SchemaFixture
      Set(f.centre, f.schema).foreach(addToRepository)
      val (url, json) = func(f.schema)

      val reqJson = Json.obj("expectedVersion" -> Some(f.schema.version + 1)) ++ json
      val reply   = makeAuthRequest(POST, url, reqJson)
      reply.value must beBadRequestWithMessage("expected version doesn't match current version")
    }

  private def listSingleSchema(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, ContainerSchema)
    ) =
    it("list single schema") {
      val (url, expectedSchema) = setupFunc()
      val reply                 = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beSingleItemResults(offset, maybeNext, maybePrev)

      val replySchemas = (json \ "data" \ "items").validate[List[ContainerSchemaDto]]
      replySchemas must be(jsSuccess)
      replySchemas.get.foreach { _ must matchDtoToContainerSchema(expectedSchema) }
    }

  private def listMultipleSchemas(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, List[ContainerSchema])
    ) =
    it("list multiple schemas") {
      val (url, expectedSchemas) = setupFunc()

      val reply = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beMultipleItemResults(offset    = offset,
                                      total     = expectedSchemas.size.toLong,
                                      maybeNext = maybeNext,
                                      maybePrev = maybePrev)

      val replySchemas = (json \ "data" \ "items").validate[List[ContainerSchemaDto]]
      replySchemas must be(jsSuccess)

      (replySchemas.get.sortWith(_.id < _.id) zip expectedSchemas.sortWith(_.id.id < _.id.id)).foreach {
        case (replySchema, expectedSchema) =>
          replySchema must matchDtoToContainerSchema(expectedSchema)
      }
    }
}
