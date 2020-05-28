package org.biobank.controllers.centres

import java.time.OffsetDateTime
import org.biobank.controllers.PagedResultsSharedSpec
import org.biobank.domain.centres._
import org.biobank.domain.participants._
import org.biobank.dto.centres._
import org.biobank.fixtures.Url
import org.biobank.matchers.PagedResultsMatchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.TableDrivenPropertyChecks._
import play.api.libs.json._
import play.api.test.Helpers._

/**
 * Tests the REST API for [[ShipmentSpecimen]]s.
 *
 * Tests for [[Shipment]]s in ShipmentsControllerSpec.scala.
 */
class ShipmentSpecimensControllerSpec
    extends ShipmentsControllerSpecFixtures with PagedResultsSharedSpec with PagedResultsMatchers {

  import org.scalatest.matchers.must.Matchers._
  import org.biobank.TestUtils._
  import org.biobank.matchers.DtoMatchers._
  import org.biobank.matchers.EntityMatchers._
  import org.biobank.matchers.JsonMatchers._

  protected val basePath = "shipments/specimens"

  private def uri(shipment: Shipment): Url = uri(shipment.id.id)

  private def uri(shipment: Shipment, path: String): Url = uri(path, shipment.id.id)

  describe("Shipment specimens REST API") {

    describe("GET /api/shipments/specimens/:id") {

      it("works for shipment with no specimens") {
        val f = createdShipmentFixture
        shipmentsReadRepository.put(f.shipmentDto).futureValue
        uri(f.shipment) must beEmptyResults
      }

      describe("works for shipment with one specimen") {
        listSingleShipmentSpecimens() { () =>
          val f        = specimensFixture(1)
          val specimen = f.specimens.head
          val shipmentSpecimen =
            factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id, specimenId = specimen.id)
          shipmentSpecimensRepository.put(shipmentSpecimen)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          (uri(f.shipment), shipmentSpecimen)
        }
      }

      describe("work for shipment with more than one specimen") {
        listMultipleShipmentSpecimens() { () =>
          val numSpecimens = 2
          val f            = specimensFixture(numSpecimens)

          val shipmentSpecimens = f.specimens.map { specimen =>
            val shipmentSpecimen =
              factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id, specimenId = specimen.id)
            shipmentSpecimensRepository.put(shipmentSpecimen)
            shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
            shipmentSpecimen
          }.toList

          (uri(f.shipment), shipmentSpecimens)
        }
      }

      it("list shipment specimens filtered by item state") {
        val numSpecimens = ShipmentSpecimen.states.size
        val f            = shipmentSpecimensFixture(numSpecimens)

        val shipmentSpecimensData =
          f.shipmentSpecimenMap.values
            .zip(ShipmentSpecimen.states)
            .map {
              case (shipmentSpecimenData, itemState) =>
                val shipmentSpecimen = shipmentSpecimenData.shipmentSpecimen.copy(state = itemState)
                shipmentSpecimensRepository.put(shipmentSpecimen)
                shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
                (itemState, shipmentSpecimen)
            }

        shipmentSpecimensData.foreach {
          case (itemState, shipmentSpecimen) =>
            val reply =
              makeAuthRequest(GET, uri(f.shipment).addQueryString(s"filter=state::$itemState")).value
            reply must beOkResponseWithJsonReply

            val json = contentAsJson(reply)
            json must beSingleItemResults()

            val dtosValidation = (json \ "data" \ "items").validate[List[ShipmentSpecimenDto]]
            dtosValidation must be(jsSuccess)
            dtosValidation.get.foreach {
              _ must matchDtoToShipmentSpecimen(shipmentSpecimen)
            }
        }
      }

      it("fail for an invalid item state for a shipment specimen") {
        val f                = createdShipmentFixture
        val invalidStateName = "state::" + nameGenerator.next[ShipmentSpecimen]
        shipmentsReadRepository.put(f.shipmentDto)
        val reply = makeAuthRequest(GET, uri(f.shipment).addQueryString(s"filter=$invalidStateName")).value
        reply must beBadRequestWithMessage("EntityCriteriaError.*state does not exist")
      }

      describe("list a single specimen when using paged query") {

        describe("for the  first specimen") {
          listSingleShipmentSpecimens(maybeNext = Some(2)) { () =>
            val f = shipmentSpecimensFixture(2)

            (uri(s"${f.shipment.id.id}?limit=1"), f.shipmentSpecimenMap.values.head.shipmentSpecimen)
          }
        }

        describe("for the  last specimen") {
          listSingleShipmentSpecimens(offset = 1, maybePrev = Some(1)) { () =>
            val f = shipmentSpecimensFixture(2)

            (uri(s"${f.shipment.id.id}?page=2&limit=1"),
             f.shipmentSpecimenMap.values.toList(1).shipmentSpecimen)
          }
        }
      }

      describe("list specimens in descending order by state") {
        listMultipleShipmentSpecimens() { () =>
          val numSpecimens = ShipmentSpecimen.states.size
          val f            = shipmentSpecimensFixture(numSpecimens)

          val shipmentSpecimens = f.shipmentSpecimenMap.values
            .zip(ShipmentSpecimen.states)
            .map {
              case (shipmentSpecimenData, itemState) =>
                val shipmentSpecimen = shipmentSpecimenData.shipmentSpecimen.copy(state = itemState)
                shipmentSpecimensRepository.put(shipmentSpecimen)
                shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
                shipmentSpecimen
            }
            .toList
            .sortWith(_.state.id > _.state.id)

          (uri(s"${f.shipment.id.id}?sort=-state"), shipmentSpecimens)
        }
      }

      describe("fail when using an invalid query parameters") {
        pagedQueryShouldFailSharedBehaviour { () =>
          val f = shipmentSpecimensFixture(0)
          uri(f.shipment)
        }
      }
    }

    describe("GET /api/shipments/specimens/:shId/:shSpcId") {

      it("get a shipment specimen") {
        val f                = shipmentSpecimensFixture(1)
        val shipmentSpecimen = f.shipmentSpecimenMap.values.head.shipmentSpecimen

        val reply = makeAuthRequest(GET, uri(f.shipment.id.id, shipmentSpecimen.id.id)).value
        reply must beOkResponseWithJsonReply

        val dto = (contentAsJson(reply) \ "data").validate[ShipmentSpecimenDto]
        dto must be(jsSuccess)
        dto.get must matchDtoToShipmentSpecimen(shipmentSpecimen)
      }

      it("fails for an invalid shipment id") {
        val f                = shipmentSpecimensFixture(1)
        val shipmentSpecimen = f.shipmentSpecimenMap.values.head.shipmentSpecimen

        val badShipment = factory.createShipment

        val reply = makeAuthRequest(GET, uri(badShipment.id.id, shipmentSpecimen.id.id)).value
        reply must beNotFoundWithMessage("IdNotFound.*shipment id")
      }

    }

    describe("GET /api/shipments/specimens/canadd/:shId/:invId") {

      it("can add a specimen inventory Id") {
        val f = specimensFixture(1)
        shipmentsReadRepository.put(f.shipmentDto)
        val specimen = f.specimens.head
        specimenRepository.put(specimen)

        val url   = uri("canadd", f.shipment.id.id, specimen.inventoryId)
        val reply = makeAuthRequest(GET, url).value
        reply must beOkResponseWithJsonReply

        val result = (contentAsJson(reply) \ "data").validate[Boolean]
        result must be(jsSuccess)
        result.get must be(true)
      }

      it("fail when adding a specimen inventory Id already in the shipment") {
        val f        = shipmentSpecimensFixture(1)
        val specimen = f.shipmentSpecimenMap.values.head.specimen

        val url   = uri("canadd", f.shipment.id.id, specimen.inventoryId)
        val reply = makeAuthRequest(GET, url).value
        reply must beBadRequestWithMessage("specimens are already in an active shipment")
      }

      it("not add a specimen inventory Id that does not exist") {
        val f = createdShipmentFixture
        shipmentsReadRepository.put(f.shipmentDto)

        val invalidInventoryId = nameGenerator.next[Specimen]
        val url                = uri("canadd", f.shipment.id.id, invalidInventoryId)
        val reply              = makeAuthRequest(GET, url).value
        reply must beNotFoundWithMessage("IdNotFound: specimen inventory ID")
      }

      it("not add a specimen inventory Id that not present at shipment's from centre") {
        val f        = specimensFixture(1)
        val specimen = f.specimens.head.copy(locationId = f.destinationCentre.locations.head.id)
        specimenRepository.put(specimen)

        val url   = uri("canadd", f.shipment.id.id, specimen.inventoryId)
        val reply = makeAuthRequest(GET, url).value
        reply must beBadRequestWithMessage("specimen not at shipment's from location")
      }

      it("fails for a specimen already in another active shipment") {
        val f           = shipmentSpecimensFixture(1)
        val specimen    = f.shipmentSpecimenMap.values.head.specimen
        val newShipment = factory.createShipment(f.originCentre, f.destinationCentre)
        val dto         = f.dtoFrom(newShipment)
        shipmentsReadRepository.put(dto)

        val url   = uri("canadd", newShipment.id.id, specimen.inventoryId)
        val reply = makeAuthRequest(GET, url).value
        reply must beBadRequestWithMessage(
          "SpecimensFoundInShipment.*specimens are already in an active shipment"
        )
      }
    }

    describe("POST /api/shipments/specimens/:id") {

      it("add a specimen to a shipment") {
        val f        = specimensFixture(1)
        val specimen = f.specimens.head
        val addJson  = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply    = makeAuthRequest(POST, uri(f.shipment), addJson).value
        reply must beOkResponseWithJsonReply

        val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
        dto must be(jsSuccess)
        dto.get must matchDtoToShipment(f.shipment)

        val repoShipmentSpecimens = shipmentSpecimensRepository.forShipment(dto.get.id)
        repoShipmentSpecimens must have size (1)

        val repoShSpc = repoShipmentSpecimens.headOption.value

        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(id         = repoShSpc.id,
                                              shipmentId = f.shipment.id,
                                              specimenId = specimen.id)
        shipmentSpecimen must matchRepositoryShipmentSpecimen
      }

      it("not add a specimen to a shipment which is not in the system") {
        val f        = specimensFixture(1)
        val shipment = factory.createShipment
        val specimen = f.specimens.head
        val addJson  = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply    = makeAuthRequest(POST, uri(shipment), addJson).value
        reply must beNotFoundWithMessage("IdNotFound.*shipment id")
      }

      it("not add a specimen to a shipment not in created state") {
        val f        = specimensFixture(1)
        val specimen = f.specimens.head

        val nonCreatedShipments = Table("non created shipments",
                                        makePackedShipment(f.shipment),
                                        makeSentShipment(f.shipment),
                                        makeReceivedShipment(f.shipment),
                                        makeUnpackedShipment(f.shipment),
                                        makeLostShipment(f.shipment))

        forAll(nonCreatedShipments) { shipment =>
          val dto = f.dtoFrom(shipment)

          info(s"${shipment.state} shipment")
          val addJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          shipmentsReadRepository.put(dto)
          shipmentsWriteRepository.put(shipment)

          val reply = makeAuthRequest(POST, uri(shipment), addJson).value
          reply must beBadRequestWithMessage("InvalidState.*shipment not created")
        }
      }

      it("not add a specimen from a different centre to a shipment") {
        val f = specimensFixture(1)
        shipmentsReadRepository.put(f.shipmentDto)
        val specimen = f.specimens.head.copy(locationId = f.destinationCentre.locations.head.id)
        specimenRepository.put(specimen)

        val addJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply   = makeAuthRequest(POST, uri(f.shipment), addJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError.*invalid centre for specimen inventory IDs")
      }
    }

    describe("alter specimens in shipments") {

      val stateData = Table(("shipment specimen states", "url path"),
                            (ShipmentSpecimen.receivedState, "received"),
                            (ShipmentSpecimen.missingState, "missing"))

      it("change state on a shipment specimen") {
        val f = specimensFixture(1)

        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        val specimen = f.specimens.head
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id, specimenId = specimen.id)

        forAll(stateData) {
          case (state, urlPath) =>
            shipmentSpecimensRepository.put(shipmentSpecimen)
            val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))

            val reply = makeAuthRequest(POST, uri(f.shipment, urlPath), reqJson).value
            reply must beOkResponseWithJsonReply

            val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
            dto must be(jsSuccess)
            dto.get must matchDtoToShipment(shipment)

            val updatedShipmentSpecimen = shipmentSpecimen
              .copy(version      = shipmentSpecimen.version + 1,
                    state        = state,
                    timeModified = Some(OffsetDateTime.now))
            updatedShipmentSpecimen must matchRepositoryShipmentSpecimen
        }
      }

      it("cannot change a shipment specimen's state if shipment is not PACKED") {
        val f = specimensFixture(1)
        val shipments = Table("shipment",
                              f.shipment,
                              makePackedShipment(f.shipment),
                              makeSentShipment(f.shipment),
                              makeReceivedShipment(f.shipment))
        forAll(shipments) { shipment =>
          val specimen = f.specimens.headOption.value
          val shipmentSpecimen =
            factory.createShipmentSpecimen.copy(shipmentId = shipment.id, specimenId = specimen.id)

          forAll(stateData) {
            case (state, urlPath) =>
              shipmentsReadRepository.put(f.dtoFrom(shipment))
              shipmentSpecimensRepository.put(shipmentSpecimen)

              val url     = uri(shipment, urlPath)
              val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
              val reply   = makeAuthRequest(POST, url, reqJson).value
              reply must beBadRequestWithMessage("InvalidState.*shipment not unpacked")
          }
        }
      }

      it("cannot change a shipment specimen's state if it's not in the shipment") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.headOption.value

        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        forAll(stateData) {
          case (state, urlPath) =>
            val url     = uri(shipment, urlPath)
            val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
            val reply   = makeAuthRequest(POST, url, reqJson).value
            reply must beBadRequestWithMessage("EntityCriteriaError.*specimens not in this shipment:")
        }
      }

      it("cannot change a shipment specimen's state if the specimen not in the system") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.headOption.value

        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        specimenRepository.remove(specimen)
        forAll(stateData) {
          case (state, urlPath) =>
            val url     = uri(shipment, urlPath)
            val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
            val reply   = makeAuthRequest(POST, url, reqJson).value
            reply must beBadRequestWithMessage("EntityCriteriaError.*invalid inventory Ids:")
        }
      }

      it("cannot change a shipment specimen's state if shipment specimen's state is not present") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.headOption.value
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = shipment.id, specimenId = specimen.id)

        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        forAll(stateData) {
          case (state, urlPath) =>
            shipmentSpecimensRepository.put(shipmentSpecimen.copy(state = state))
            val url     = uri(shipment, urlPath)
            val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
            val reply   = makeAuthRequest(POST, url, reqJson).value
            reply must beBadRequestWithMessage("EntityCriteriaError.*shipment specimens not present:")
        }
      }

      it("change a shipment specimen's state to PRESENT from another state") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.headOption.value
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = shipment.id, specimenId = specimen.id)

        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        forAll(stateData) {
          case (state, urlPath) =>
            shipmentSpecimensRepository.put(shipmentSpecimen.copy(state = state))

            val url     = uri(shipment, "present")
            val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))

            val reply = makeAuthRequest(POST, url, reqJson).value
            reply must beOkResponseWithJsonReply

            val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
            dto must be(jsSuccess)
            dto.get must matchDtoToShipment(shipment)

            val updatedShipmentSpecimen = shipmentSpecimen
              .copy(version      = shipmentSpecimen.version + 1,
                    state        = ShipmentSpecimen.presentState,
                    timeModified = Some(OffsetDateTime.now))
            updatedShipmentSpecimen must matchRepositoryShipmentSpecimen
        }
      }

      it("fail when changing a shipment specimen's state to PRESENT when it is already PRESENT") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.headOption.value
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = shipment.id, specimenId = specimen.id)

        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        shipmentSpecimensRepository.put(shipmentSpecimen.copy(state = ShipmentSpecimen.presentState))

        val url     = uri(shipment, "present")
        val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply   = makeAuthRequest(POST, url, reqJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError.*shipment specimens are present:")
      }

      it("add a shipment specimen as EXTRA to a shipment") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.headOption.value
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        val url     = uri(shipment, "extra")
        val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply   = makeAuthRequest(POST, url, reqJson).value
        reply must beOkResponseWithJsonReply

        val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
        dto must be(jsSuccess)
        dto.get must matchDtoToShipment(shipment)

        val repoShipmentSpecimens = shipmentSpecimensRepository.forShipment(dto.get.id)
        repoShipmentSpecimens must have size (1)
        repoShipmentSpecimens.foreach { _.specimenId must be(specimen.id) }
      }

      it("not add an EXTRA shipment specimen to a shipment if it is present in another shipment") {
        val f        = specimensFixture(1)
        val f2       = createdShipmentFixture
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        val specimen = f.specimens.headOption.value
        specimenRepository.put(specimen)

        // this shipment specimen belongs to a different shipment
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(shipmentId = f2.shipment.id,
                                                                   specimenId = specimen.id,
                                                                   state      = ShipmentSpecimen.presentState)
        shipmentSpecimensRepository.put(shipmentSpecimen)
        val url     = uri(shipment, "extra")
        val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply   = makeAuthRequest(POST, url, reqJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError.*specimens are already in an active shipment")
      }

      it("not add an EXTRA shipment specimen to a shipment if it is already part of the shipment") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        val specimen = f.specimens.headOption.value
        specimenRepository.put(specimen)
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = shipment.id, specimenId = specimen.id)
        forAll(stateData) {
          case (state, urlPath) =>
            shipmentSpecimensRepository.put(shipmentSpecimen.copy(state = state))
            val url     = uri(shipment, "extra")
            val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
            val reply   = makeAuthRequest(POST, url, reqJson).value
            reply must beBadRequestWithMessage(
              "EntityCriteriaError.*specimen inventory IDs already in this shipment: "
            )
        }
      }

      it("not add an EXTRA shipment specimen to a shipment if specimen at a different centre") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        val specimen = f.specimens.headOption.value.copy(locationId = f.destinationCentre.locations.head.id)
        specimenRepository.put(specimen)

        val url     = uri(shipment, "extra")
        val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
        val reply   = makeAuthRequest(POST, url, reqJson).value
        reply must beBadRequestWithMessage("EntityCriteriaError.*invalid centre for specimen inventory IDs")
      }

    }

    describe("POST /api/shipments/specimens/present/:id") {
      it("can tag a non present specimen as present") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val shipmentSpecimen = v.shipmentSpecimen.copy(state = ShipmentSpecimen.missingState)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          shipmentSpecimensRepository.put(shipmentSpecimen)

          val url     = uri(shipment, "present")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beOkResponseWithJsonReply

          val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
          dto must be(jsSuccess)
          dto.get must matchDtoToShipment(shipment)

          val repoShipmentSpecimens = shipmentSpecimensRepository.forShipment(dto.get.id)
          repoShipmentSpecimens must have size (1)
          repoShipmentSpecimens.foreach { r =>
            r.specimenId must be(specimen.id)
            r.state must be(ShipmentSpecimen.presentState)
          }
        }
      }

      it("replies whith an error if the specimen is already present") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val shipmentSpecimen = v.shipmentSpecimen.copy(state = ShipmentSpecimen.presentState)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          shipmentSpecimensRepository.put(shipmentSpecimen)

          val url     = uri(shipment, "present")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage("EntityCriteriaError.*shipment specimens are present")
        }
      }

      it("replies with an error when attempting to tag a specimen not in the shipment") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = factory.createUsableSpecimen
          specimenRepository.put(specimen)

          val url     = uri(shipment, "present")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage("EntityCriteriaError.*specimens not in this shipment")
        }
      }
    }

    describe("POST /api/shipments/specimens/received/:id") {
      it("can tag a present specimen as received") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val shipmentSpecimen = v.shipmentSpecimen.copy(state = ShipmentSpecimen.presentState)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          shipmentSpecimensRepository.put(shipmentSpecimen)

          val url     = uri(shipment, "received")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beOkResponseWithJsonReply

          val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
          dto must be(jsSuccess)
          dto.get must matchDtoToShipment(shipment)

          val repoShipmentSpecimens = shipmentSpecimensRepository.forShipment(dto.get.id)
          repoShipmentSpecimens must have size (1)
          repoShipmentSpecimens.foreach { r =>
            r.specimenId must be(specimen.id)
            r.state must be(ShipmentSpecimen.receivedState)
          }
        }
      }

      it("replies whith an error if the specimen is already received") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val shipmentSpecimen = v.shipmentSpecimen.copy(state = ShipmentSpecimen.receivedState)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          shipmentSpecimensRepository.put(shipmentSpecimen)

          val url     = uri(shipment, "received")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage("EntityCriteriaError.*shipment specimens not present")
        }
      }

      it("replies with an error when attempting to tag a specimen not in the shipment") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = factory.createUsableSpecimen
          specimenRepository.put(specimen)

          val url     = uri(shipment, "received")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage("EntityCriteriaError.*specimens not in this shipment")
        }
      }
    }

    describe("POST /api/shipments/specimens/missing/:id") {
      it("can tag a present specimen as missing") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val shipmentSpecimen = v.shipmentSpecimen.copy(state = ShipmentSpecimen.presentState)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          shipmentSpecimensRepository.put(shipmentSpecimen)

          val url     = uri(shipment, "missing")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beOkResponseWithJsonReply

          val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
          dto must be(jsSuccess)
          dto.get must matchDtoToShipment(shipment)

          val repoShipmentSpecimens = shipmentSpecimensRepository.forShipment(dto.get.id)
          repoShipmentSpecimens must have size (1)
          repoShipmentSpecimens.foreach { r =>
            r.specimenId must be(specimen.id)
            r.state must be(ShipmentSpecimen.missingState)
          }
        }
      }

      it("replies whith an error if the specimen is already missing") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val shipmentSpecimen = v.shipmentSpecimen.copy(state = ShipmentSpecimen.missingState)
          shipmentSpecimensReadRepository.put(ShipmentSpecimenDto.from(shipmentSpecimen))
          shipmentSpecimensRepository.put(shipmentSpecimen)

          val url     = uri(shipment, "missing")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage("EntityCriteriaError.*shipment specimens not present")
        }
      }

      it("replies with an error when attempting to tag a specimen not in the shipment") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = factory.createUsableSpecimen
          specimenRepository.put(specimen)

          val url     = uri(shipment, "received")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage("EntityCriteriaError.*specimens not in this shipment")
        }
      }
    }

    describe("POST /api/shipments/specimens/extra/:id") {
      it("can tag a specimen as extra") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val extraSpecimen = factory.createUsableSpecimen.copy(originLocationId = f.originLocation.id,
                                                                locationId = f.originLocation.id)
          specimenRepository.put(extraSpecimen)
          ceventSpecimenRepository.put(CeventSpecimen(f.cevent.id, extraSpecimen.id))

          val url     = uri(shipment, "extra")
          val reqJson = Json.obj("specimenInventoryIds" -> List(extraSpecimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beOkResponseWithJsonReply

          val dto = (contentAsJson(reply) \ "data").validate[ShipmentDto]
          dto must be(jsSuccess)
          dto.get must matchDtoToShipment(shipment)

          shipmentSpecimensRepository.getBySpecimen(shipment.id, extraSpecimen) mustSucceed {
            _.state must be(ShipmentSpecimen.extraState)
          }
        }
      }

      it("replies with an error if the specimen is already present in another shipment") {
        val f        = shipmentSpecimensFixture(1)
        val f2       = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val extraSpecimen = f2.specimens.headOption.value
          specimenRepository.put(extraSpecimen)

          val url     = uri(shipment, "extra")
          val reqJson = Json.obj("specimenInventoryIds" -> List(extraSpecimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage(
            "EntityCriteriaError.*specimens are already in an active shipment"
          )
        }
      }

      it("replies with an error when attempting to add an extra specimen that is already in the shipment") {
        val f        = shipmentSpecimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        f.shipmentSpecimenMap.values.foreach { v =>
          val specimen = v.specimen
          specimenRepository.put(specimen)

          val url     = uri(shipment, "extra")
          val reqJson = Json.obj("specimenInventoryIds" -> List(specimen.inventoryId))
          val reply   = makeAuthRequest(POST, url, reqJson).value
          reply must beBadRequestWithMessage(
            "EntityCriteriaError.*specimen inventory IDs already in this shipment"
          )
        }
      }
    }

    describe("DELETE /api/shipments/specimens/:shId/:shSpcId/:ver") {

      it("must remove a specimen from shipment in created state") {
        val f = specimensFixture(1)

        val specimen = f.specimens.head
        val shipmentSpecimen =
          factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id, specimenId = specimen.id)
        shipmentSpecimensRepository.put(shipmentSpecimen)
        val url   = uri(f.shipment.id.id, shipmentSpecimen.id.id, shipmentSpecimen.version.toString)
        val reply = makeAuthRequest(DELETE, url).value
        reply must beOkResponseWithJsonReply
        shipmentSpecimensRepository.getByKey(shipmentSpecimen.id) mustBe None
      }

      it("must remove an extra specimen from shipment in unpacked state") {
        val f        = specimensFixture(1)
        val shipment = makeUnpackedShipment(f.shipment)
        val specimen = f.specimens.head
        val shipmentSpecimen = factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id,
                                                                   specimenId = specimen.id,
                                                                   state      = ShipmentSpecimen.extraState)
        shipmentsReadRepository.put(f.dtoFrom(shipment))
        shipmentsWriteRepository.put(shipment)

        shipmentSpecimensRepository.put(shipmentSpecimen)
        val url   = uri(f.shipment.id.id, shipmentSpecimen.id.id, shipmentSpecimen.version.toString)
        val reply = makeAuthRequest(DELETE, url).value
        reply must beOkResponseWithJsonReply
        shipmentSpecimensRepository.getByKey(shipmentSpecimen.id) mustBe None
      }

      it("must not delete a specimen from a shipment not in created or unpacked state") {
        val f        = specimensFixture(1)
        val specimen = f.specimens.head
        val shipments = Table("shipment",
                              makePackedShipment(f.shipment),
                              makeSentShipment(f.shipment),
                              makeReceivedShipment(f.shipment),
                              makeLostShipment(f.shipment))
        val stateData =
          Table("shipment specimen states", ShipmentSpecimen.receivedState, ShipmentSpecimen.missingState)

        forAll(shipments) { shipment =>
          forAll(stateData) { shipSpecimenState =>
            info(s"shipment state: ${shipment.state}, shipment specimen state: $shipSpecimenState")
            shipmentsReadRepository.put(f.dtoFrom(shipment))
            val shipmentSpecimen = factory.createShipmentSpecimen.copy(shipmentId = shipment.id,
                                                                       specimenId = specimen.id,
                                                                       state      = shipSpecimenState)

            shipmentSpecimensRepository.put(shipmentSpecimen)
            val url   = uri(shipment.id.id, shipmentSpecimen.id.id, shipmentSpecimen.version.toString)
            val reply = makeAuthRequest(DELETE, url).value
            reply must beBadRequestWithMessage(
              "EntityCriteriaError.*cannot remove, shipment specimen state is invalid"
            )

            shipmentSpecimensRepository.getByKey(shipmentSpecimen.id) must not be None
          }
        }
      }

    }

  }

  private def listSingleShipmentSpecimens(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, ShipmentSpecimen)
    ) =
    it("list single shipment") {
      val (url, expectedShipmentSpecimen) = setupFunc()
      val reply                           = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beSingleItemResults(offset, maybeNext, maybePrev)

      val dtosValidation = (json \ "data" \ "items").validate[List[ShipmentSpecimenDto]]
      dtosValidation must be(jsSuccess)
      dtosValidation.get.foreach {
        _ must matchDtoToShipmentSpecimen(expectedShipmentSpecimen)
      }
    }

  private def listMultipleShipmentSpecimens(
      offset:    Long = 0,
      maybeNext: Option[Int] = None,
      maybePrev: Option[Int] = None
    )(setupFunc: () => (Url, List[ShipmentSpecimen])
    ) =
    it("list multiple shipments") {
      val (url, expectedShipmentSpecimens) = setupFunc()

      val reply = makeAuthRequest(GET, url).value
      reply must beOkResponseWithJsonReply

      val json = contentAsJson(reply)
      json must beMultipleItemResults(offset    = offset,
                                      total     = expectedShipmentSpecimens.size.toLong,
                                      maybeNext = maybeNext,
                                      maybePrev = maybePrev)

      val dtosValidation = (json \ "data" \ "items").validate[List[ShipmentSpecimenDto]]
      dtosValidation must be(jsSuccess)

      (dtosValidation.get zip expectedShipmentSpecimens).foreach {
        case (dto, expectedShipmentSpecimen) =>
          dto must matchDtoToShipmentSpecimen(expectedShipmentSpecimen)
      }
    }

  def matchRepositoryShipmentSpecimen =
    new Matcher[ShipmentSpecimen] {

      def apply(left: ShipmentSpecimen) = {
        shipmentSpecimensRepository.getByKey(left.id) match {
          case None => MatchResult(false, s"not found in repository: ${left.id}", "")
          case Some(repoSs) =>
            val repoMatcher = matchShipmentSpecimen(left)(repoSs)
            MatchResult(repoMatcher.matches,
                        s"repository shipment specimen does not match expected: ${repoMatcher.failureMessage}",
                        s"repository shipment specimen matches expected: ${repoMatcher.failureMessage}")
        }
      }

    }

}
