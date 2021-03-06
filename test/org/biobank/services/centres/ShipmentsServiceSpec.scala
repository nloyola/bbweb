package org.biobank.services.centres

import java.time.OffsetDateTime
import org.biobank.fixtures._
import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.studies._
import org.biobank.domain.participants._
import org.biobank.domain.users._
import org.biobank.query.centres._
import org.biobank.services.{FilterString, PagedQuery, SortString}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.TableDrivenPropertyChecks._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Primarily these are tests that exercise the User Access aspect of SpecimenService.
 */
class ShipmentsServiceSpec extends CentresServiceFixtures with ShipmentSpecFixtures with ScalaFutures {

  import org.biobank.TestUtils._
  import org.biobank.infrastructure.commands.ShipmentCommands._
  import org.biobank.infrastructure.commands.ShipmentSpecimenCommands._

  class UsersWithShipmentAccessFixture {
    val originLocation      = factory.createLocation
    val destinationLocation = factory.createLocation
    val originCentre        = factory.createEnabledCentre.copy(locations = Set(originLocation))
    val destinationCentre   = factory.createEnabledCentre.copy(locations = Set(destinationLocation))
    val shipment            = factory.createShipment(originCentre, destinationCentre)

    val allCentresAdminUser         = factory.createActiveUser
    val centreOnlyShippingAdminUser = factory.createActiveUser
    val shippingUser                = factory.createActiveUser
    val noMembershipUser            = factory.createActiveUser
    val noShippingPermissionUser    = factory.createActiveUser

    val allCentresMembership = factory.createMembership.copy(userIds = Set(allCentresAdminUser.id),
                                                             centreData =
                                                               MembershipEntitySet(true, Set.empty[CentreId]))

    val centreOnlyMembership = factory.createMembership.copy(
      userIds    = Set(centreOnlyShippingAdminUser.id, shippingUser.id),
      centreData = MembershipEntitySet(false, Set(originCentre.id, destinationCentre.id))
    )

    val noCentresMembership = factory.createMembership.copy(
      userIds    = Set(noMembershipUser.id, noShippingPermissionUser.id),
      centreData = MembershipEntitySet(false, Set.empty[CentreId])
    )

    val usersCanReadTable = Table(("users with read access", "label"),
                                  (allCentresAdminUser, "all centres admin user"),
                                  (centreOnlyShippingAdminUser, "centre only shipping admin user"),
                                  (shippingUser, "non-admin shipping user"))

    val usersCanAddOrUpdateTable = Table(("users with update access", "label"),
                                         (allCentresAdminUser, "all centres admin user"),
                                         (centreOnlyShippingAdminUser, "centre only shipping admin user"),
                                         (shippingUser, "non-admin centre user"))

    val usersCannotAddOrUpdateTable = Table(("users with update access", "label"),
                                            (noMembershipUser, "no memberships user"),
                                            (noShippingPermissionUser, "no shipping permission user"))

    val usersWithoutAccess = Table(("users that can remove specimens", "label"),
                                   (noMembershipUser, "no memberships user"),
                                   (noShippingPermissionUser, "no shipping permission user"))

    Set(originCentre,
        destinationCentre,
        shipment,
        allCentresAdminUser,
        centreOnlyShippingAdminUser,
        shippingUser,
        noMembershipUser,
        noShippingPermissionUser,
        allCentresMembership,
        centreOnlyMembership,
        noCentresMembership).foreach(addToRepository)

    Await.ready(shipmentsReadRepository.put(shipment), Duration.Inf)

    addUserToRole(allCentresAdminUser, RoleId.ShippingAdministrator)
    addUserToRole(centreOnlyShippingAdminUser, RoleId.ShippingAdministrator)
    addUserToRole(shippingUser, RoleId.ShippingUser)
    addUserToRole(noMembershipUser, RoleId.ShippingUser)
  }

  protected val nameGenerator = new NameGenerator(this.getClass)

  protected val accessItemRepository = app.injector.instanceOf[AccessItemRepository]

  protected val membershipRepository = app.injector.instanceOf[MembershipRepository]

  protected val userRepository = app.injector.instanceOf[UserRepository]

  protected val centreRepository = app.injector.instanceOf[CentreRepository]

  protected val studyRepository = app.injector.instanceOf[StudyRepository]

  protected val collectionEventTypeRepository = app.injector.instanceOf[CollectionEventTypeRepository]

  protected val participantRepository = app.injector.instanceOf[ParticipantRepository]

  protected val collectionEventRepository = app.injector.instanceOf[CollectionEventRepository]

  protected val specimenRepository = app.injector.instanceOf[SpecimenRepository]

  protected val ceventSpecimenRepository = app.injector.instanceOf[CeventSpecimenRepository]

  protected val shipmentsWriteRepository = app.injector.instanceOf[ShipmentsWriteRepository]

  protected val shipmentSpecimensRepository = app.injector.instanceOf[ShipmentSpecimensWriteRepository]

  private val shipmentsService = app.injector.instanceOf[ShipmentsService]

  private val shipmentsReadRepository = app.injector.instanceOf[ShipmentsReadRepository]

  private val shipmentSpecimensReadRepository = app.injector.instanceOf[ShipmentSpecimensReadRepository]

  override protected def addToRepository[T <: ConcurrencySafeEntity[_]](entity: T): Unit = {
    entity match {
      case e: Shipment =>
        shipmentsWriteRepository.put(e)
        shipmentsReadRepository.put(e)

      case e: ShipmentSpecimen    => shipmentSpecimensRepository.put(e)
      case e: Specimen            => specimenRepository.put(e)
      case e: CollectionEventType => collectionEventTypeRepository.put(e)
      case e: Participant         => participantRepository.put(e)
      case e: CollectionEvent     => collectionEventRepository.put(e)
      case e => super.addToRepository(e)
    }
    ()
  }

  private def getAddShipmentCmd(userId: UserId, shipment: CreatedShipment) =
    AddShipmentCmd(sessionUserId         = userId.id,
                   courierName           = shipment.courierName,
                   trackingNumber        = shipment.trackingNumber,
                   originLocationId      = shipment.originLocationId.id,
                   destinationLocationId = shipment.destinationLocationId.id)

  private def getRemoveShipmentCmd(userId: UserId, shipment: CreatedShipment) =
    ShipmentRemoveCmd(sessionUserId = userId.id, id = shipment.id.id, expectedVersion = shipment.version)

  def shipmentSpecimenFixture() = {
    val f             = new UsersWithShipmentAccessFixture
    val ceventFixture = new CollectionEventFixture
    val specimen =
      factory.createUsableSpecimen
        .copy(originLocationId = f.originLocation.id, locationId = f.originLocation.id)
    val shipmentSpecimen =
      factory.createShipmentSpecimen.copy(shipmentId = f.shipment.id, specimenId = specimen.id)

    Set(f.allCentresMembership, f.centreOnlyMembership).foreach { membership =>
      addToRepository(membership.copy(studyData = MembershipEntitySet(false, Set(ceventFixture.study.id))))
    }

    ceventSpecimenRepository.put(CeventSpecimen(ceventFixture.cevent.id, specimen.id))
    Set(ceventFixture.study,
        ceventFixture.ceventType,
        ceventFixture.participant,
        ceventFixture.cevent,
        specimen,
        shipmentSpecimen).foreach(addToRepository)

    Await.ready(shipmentSpecimensReadRepository.put(shipmentSpecimen), Duration.Inf)

    (f, specimen, shipmentSpecimen)
  }

  private def updateCommandsTable(sessionUserId: UserId, shipment: Shipment) =
    Table("shipment update commands",
          UpdateShipmentCourierNameCmd(sessionUserId      = sessionUserId.id,
                                       id                 = shipment.id.id,
                                       expectedVersion    = shipment.version,
                                       courierName        = shipment.courierName),
          UpdateShipmentTrackingNumberCmd(sessionUserId   = sessionUserId.id,
                                          id              = shipment.id.id,
                                          expectedVersion = shipment.version,
                                          trackingNumber  = shipment.trackingNumber),
          UpdateShipmentOriginCmd(sessionUserId           = sessionUserId.id,
                                  id                      = shipment.id.id,
                                  expectedVersion         = shipment.version,
                                  locationId              = shipment.originLocationId.id),
          UpdateShipmentDestinationCmd(sessionUserId      = sessionUserId.id,
                                       id                 = shipment.id.id,
                                       expectedVersion    = shipment.version,
                                       locationId         = shipment.destinationLocationId.id))

  private def changeStateCommandsTable(sessionUserId: UserId, shipment: Shipment) = {
    val shipments = Map((Shipment.createdState, shipment),
                        (Shipment.packedState, makePackedShipment(shipment)),
                        (Shipment.sentState, makeSentShipment(shipment)),
                        (Shipment.receivedState, makeReceivedShipment(shipment)),
                        (Shipment.unpackedState, makeUnpackedShipment(shipment)),
                        (Shipment.lostState, makeLostShipment(shipment)))

    Table(("shipment", "command"),
          (shipments(Shipment.packedState),
           CreatedShipmentCmd(sessionUserId   = sessionUserId.id,
                              id              = shipments(Shipment.packedState).id.id,
                              expectedVersion = shipments(Shipment.packedState).version)),
          (shipments(Shipment.createdState),
           PackShipmentCmd(sessionUserId   = sessionUserId.id,
                           id              = shipments(Shipment.createdState).id.id,
                           expectedVersion = shipments(Shipment.createdState).version,
                           datetime        = OffsetDateTime.now)),
          (shipments(Shipment.packedState),
           SendShipmentCmd(sessionUserId   = sessionUserId.id,
                           id              = shipments(Shipment.packedState).id.id,
                           expectedVersion = shipments(Shipment.packedState).version,
                           datetime        = OffsetDateTime.now)),
          (shipments(Shipment.sentState),
           ReceiveShipmentCmd(sessionUserId   = sessionUserId.id,
                              id              = shipments(Shipment.sentState).id.id,
                              expectedVersion = shipments(Shipment.sentState).version,
                              datetime        = OffsetDateTime.now)),
          (shipments(Shipment.receivedState),
           UnpackShipmentCmd(sessionUserId   = sessionUserId.id,
                             id              = shipments(Shipment.receivedState).id.id,
                             expectedVersion = shipments(Shipment.receivedState).version,
                             datetime        = OffsetDateTime.now)),
          (shipments(Shipment.unpackedState),
           CompleteShipmentCmd(sessionUserId   = sessionUserId.id,
                               id              = shipments(Shipment.unpackedState).id.id,
                               expectedVersion = shipments(Shipment.unpackedState).version,
                               datetime        = OffsetDateTime.now)),
          (shipments(Shipment.sentState),
           LostShipmentCmd(sessionUserId   = sessionUserId.id,
                           id              = shipments(Shipment.sentState).id.id,
                           expectedVersion = shipments(Shipment.sentState).version)),
          (shipments(Shipment.createdState),
           ShipmentSkipStateToSentCmd(sessionUserId   = sessionUserId.id,
                                      id              = shipments(Shipment.createdState).id.id,
                                      expectedVersion = shipments(Shipment.createdState).version,
                                      timePacked      = OffsetDateTime.now,
                                      timeSent        = OffsetDateTime.now)),
          (shipments(Shipment.sentState),
           ShipmentSkipStateToUnpackedCmd(sessionUserId   = sessionUserId.id,
                                          id              = shipments(Shipment.sentState).id.id,
                                          expectedVersion = shipments(Shipment.sentState).version,
                                          timeReceived    = OffsetDateTime.now,
                                          timeUnpacked    = OffsetDateTime.now)))
  }

  private def shipmentSpecimenCommandsTable(
      sessionUserId:    UserId,
      shipment:         Shipment,
      specimen:         Specimen,
      shipmentSpecimen: ShipmentSpecimen
    ) = {
    val shipments =
      Map((Shipment.createdState, shipment), (Shipment.unpackedState, makeUnpackedShipment(shipment)))

    Table(("shipment", "shipment specimen command"),
          (shipments(Shipment.createdState),
           ShipmentAddSpecimensCmd(sessionUserId        = sessionUserId.id,
                                   shipmentId           = shipmentSpecimen.shipmentId.id,
                                   shipmentContainerId  = None,
                                   specimenInventoryIds = List(specimen.inventoryId))),
          (shipments(Shipment.createdState),
           ShipmentSpecimenRemoveCmd(sessionUserId      = sessionUserId.id,
                                     shipmentId         = shipmentSpecimen.shipmentId.id,
                                     expectedVersion    = shipmentSpecimen.version,
                                     shipmentSpecimenId = shipmentSpecimen.id.id)
           // ),(
           // this command has not been implemented yet
           //
           // ShipmentSpecimenUpdateContainerCmd(
           //   sessionUserId         = sessionUserId.id,
           //   shipmentId            = shipment.id.id,
           //   shipmentContainerId   = None,
           //   specimenInventoryIds  = List(specimen.inventoryId)
           // ),
          ),
          (shipments(Shipment.unpackedState),
           ShipmentSpecimensPresentCmd(sessionUserId        = sessionUserId.id,
                                       shipmentId           = shipmentSpecimen.shipmentId.id,
                                       specimenInventoryIds = List(specimen.inventoryId))),
          (shipments(Shipment.unpackedState),
           ShipmentSpecimensReceiveCmd(sessionUserId        = sessionUserId.id,
                                       shipmentId           = shipmentSpecimen.shipmentId.id,
                                       specimenInventoryIds = List(specimen.inventoryId))),
          (shipments(Shipment.unpackedState),
           ShipmentSpecimenMissingCmd(sessionUserId        = sessionUserId.id,
                                      shipmentId           = shipmentSpecimen.shipmentId.id,
                                      specimenInventoryIds = List(specimen.inventoryId))),
          (shipments(Shipment.unpackedState),
           ShipmentSpecimenExtraCmd(sessionUserId        = sessionUserId.id,
                                    shipmentId           = shipmentSpecimen.shipmentId.id,
                                    specimenInventoryIds = List(specimen.inventoryId))))
  }

  override def beforeEach() = {
    super.beforeEach()
    collectionEventTypeRepository.removeAll
    participantRepository.removeAll
    collectionEventRepository.removeAll
    specimenRepository.removeAll
    centreRepository.removeAll
    shipmentsReadRepository.removeAll
    shipmentsWriteRepository.removeAll
    shipmentSpecimensRepository.removeAll
  }

  describe("SpecimenService") {

    describe("when getting a shipment") {

      it("users can access - 1") {
        val f = new UsersWithShipmentAccessFixture

        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          shipmentsService.getShipment(user.id, f.shipment.id) mustSucceed { dto =>
            dto.id must be(f.shipment.id.id)
          }
        }
      }

      it("users cannot access") {
        val f = new UsersWithShipmentAccessFixture
        info("no membership user")
        shipmentsService
          .getShipment(f.noMembershipUser.id, f.shipment.id)
          .mustFail("Unauthorized")

        info("no permission user")
        shipmentsService
          .getShipment(f.noShippingPermissionUser.id, f.shipment.id)
          .mustFail("Unauthorized")

      }

    }

    describe("when listing shipments") {

      it("users can access") {
        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        val f     = new UsersWithShipmentAccessFixture
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          shipmentsService.getShipments(user.id, query) mustSucceed { result =>
            result.items must have size 1
          }
        }
      }

      ignore("users cannot access") {
        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        val f     = new UsersWithShipmentAccessFixture
        info("no membership user")
        shipmentsService.getShipments(f.noMembershipUser.id, query) mustSucceed { result =>
          result.items must have size 0
        }

        info("no permission user")
        shipmentsService
          .getShipments(f.noShippingPermissionUser.id, query)
          .mustFail("Unauthorized")
      }

    }

    describe("when getting a shipment specimen") {

      ignore("users can access") {
        val (f, _, shipmentSpecimen) = shipmentSpecimenFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          shipmentsService
            .getShipmentSpecimen(user.id, f.shipment.id, shipmentSpecimen.id)
            .mustSucceed { _.id must be(shipmentSpecimen.id.id) }
        }
      }

      it("users cannot access") {
        val (f, _, shipmentSpecimen) = shipmentSpecimenFixture
        forAll(f.usersWithoutAccess) { (user, label) =>
          info("label")
          shipmentsService
            .getShipmentSpecimen(f.noMembershipUser.id, f.shipment.id, shipmentSpecimen.id)
            .mustFail("Unauthorized")
        }

      }

    }

    describe("when listing shipment specimens") {

      ignore("users can access") {
        val (f, _, _) = shipmentSpecimenFixture
        val query     = PagedQuery(new FilterString(""), new SortString(""), 0, 1)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          shipmentsService
            .getShipmentSpecimens(user.id, f.shipment.id, query)
            .mustSucceed { reply =>
              reply.items must have size 1
            }
        }
      }

      it("users cannot access") {
        val (f, _, _) = shipmentSpecimenFixture
        val query     = PagedQuery(new FilterString(""), new SortString(""), 0, 1)

        info("no membership user")
        shipmentsService
          .getShipmentSpecimens(f.noMembershipUser.id, f.shipment.id, query)
          .mustFail("Unauthorized")

        info("no permission user")
        shipmentsService
          .getShipmentSpecimens(f.noShippingPermissionUser.id, f.shipment.id, query)
          .mustFail("Unauthorized")
      }

    }

    describe("when determining if a specimen can be added to a shipment") {

      ignore("users can access") {
        val (f, specimen, shipmentSpecimen) = shipmentSpecimenFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          shipmentSpecimensRepository.remove(shipmentSpecimen)
          shipmentSpecimensReadRepository.remove(shipmentSpecimen.id)
          shipmentsService
            .shipmentCanAddSpecimen(user.id, f.shipment.id, specimen.inventoryId)
            .mustSucceed { reply =>
              reply.inventoryId must be(specimen.inventoryId)
            }
        }
      }

      it("users cannot access") {
        val (f, specimen, shipmentSpecimen) = shipmentSpecimenFixture
        shipmentSpecimensRepository.remove(shipmentSpecimen)

        forAll(f.usersWithoutAccess) { (user, label) =>
          info(label)
          shipmentsService
            .shipmentCanAddSpecimen(user.id, f.shipment.id, specimen.inventoryId)
            .mustFail("Unauthorized")
        }
      }

    }

    describe("when adding a shipment") {

      it("users can access") {
        val f = new UsersWithShipmentAccessFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          val cmd = getAddShipmentCmd(user.id, f.shipment)
          shipmentsReadRepository.removeAll.futureValue
          shipmentsWriteRepository.removeAll
          shipmentsService.processCommand(cmd) mustSucceed { reply =>
            reply.courierName must be(f.shipment.courierName)
          }
        }
      }

      it("users cannot access") {
        val f = new UsersWithShipmentAccessFixture
        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          val cmd = getAddShipmentCmd(user.id, f.shipment)
          shipmentsReadRepository.removeAll.futureValue
          shipmentsWriteRepository.removeAll
          shipmentsService.processCommand(cmd) mustFail "Unauthorized"
        }
      }

    }

    describe("when updating a shipment") {

      it("users with access") {
        val f = new UsersWithShipmentAccessFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(updateCommandsTable(user.id, f.shipment)) { cmd =>
            shipmentsWriteRepository.put(f.shipment) // restore it to it's previous state

            shipmentsService.processCommand(cmd).mustSucceed { _.id must be(f.shipment.id.id) }
          }
        }
      }

      it("users without access") {
        val f = new UsersWithShipmentAccessFixture
        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          forAll(updateCommandsTable(user.id, f.shipment)) { cmd =>
            shipmentsService.processCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

    describe("when changing state on a shipment") {

      it("users with access") {
        val (f, _, shipmentSpecimen) = shipmentSpecimenFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(changeStateCommandsTable(user.id, f.shipment)) { (shipment, cmd) =>
            shipment match {
              case s: UnpackedShipment =>
                addToRepository(shipmentSpecimen.copy(state = ShipmentItemState.Received))
              case _ =>
                addToRepository(shipmentSpecimen.copy(state = ShipmentItemState.Present))
            }

            addToRepository(shipment)
            shipmentsService.processCommand(cmd) mustSucceed { _.id must be(shipment.id.id) }
          }
        }
      }

      it("users without access") {
        val (f, _, _) = shipmentSpecimenFixture
        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(changeStateCommandsTable(user.id, f.shipment)) { (shipment, cmd) =>
            addToRepository(f.shipment)
            shipmentsService.processCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

    describe("when updating shipment specimens") {

      it("users with access") {
        val (f, specimen, shipmentSpecimen) = shipmentSpecimenFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(shipmentSpecimenCommandsTable(user.id, f.shipment, specimen, shipmentSpecimen)) {
            (shipment, cmd) =>
              // set up pre-condition
              addToRepository(shipment)
              shipmentSpecimensRepository.removeAll
              cmd match {
                case _: ShipmentAddSpecimensCmd | _: ShipmentSpecimenExtraCmd =>
                case c: ShipmentSpecimensPresentCmd =>
                  shipmentSpecimensRepository.put(shipmentSpecimen.copy(state = ShipmentItemState.Received))
                case c =>
                  shipmentSpecimensRepository.put(shipmentSpecimen)
              }

              val v = shipmentsService.processShipmentSpecimenCommand(cmd)
              v mustSucceed { _.id must be(f.shipment.id.id) }
          }
        }
      }

      it("users without access") {
        val (f, specimen, shipmentSpecimen) = shipmentSpecimenFixture
        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(shipmentSpecimenCommandsTable(user.id, f.shipment, specimen, shipmentSpecimen)) {
            (shipment, cmd) =>
              // set up pre-condition
              addToRepository(shipment)
              shipmentSpecimensRepository.removeAll
              cmd match {
                case _: ShipmentAddSpecimensCmd | _: ShipmentSpecimenExtraCmd =>
                case c: ShipmentSpecimensPresentCmd =>
                  shipmentSpecimensRepository.put(shipmentSpecimen.copy(state = ShipmentItemState.Received))
                case c =>
                  shipmentSpecimensRepository.put(shipmentSpecimen)
              }

              shipmentsService.processShipmentSpecimenCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

    describe("when removing a shipment") {

      it("users with access") {
        val f = new UsersWithShipmentAccessFixture

        val usersCanRemove = Table(("users that can remove specimens", "label"),
                                   (f.allCentresAdminUser, "all centres admin user"),
                                   (f.centreOnlyShippingAdminUser, "centre only shipping admin user"))

        forAll(usersCanRemove) { (user, label) =>
          info(label)
          val cmd = getRemoveShipmentCmd(user.id, f.shipment)
          shipmentsWriteRepository.put(f.shipment) // restore it to it's previous state
          shipmentsService.removeShipment(cmd) mustSucceed { reply =>
            ()
          }
        }
      }

      it("users without access") {
        val f = new UsersWithShipmentAccessFixture

        val usersCannotRemove = Table(("users that can remove specimens", "label"),
                                      (f.shippingUser, "non-admin shipping user"),
                                      (f.noMembershipUser, "no memberships user"),
                                      (f.noShippingPermissionUser, "no shipping permission user"))

        forAll(usersCannotRemove) { (user, label) =>
          info(label)
          val cmd = getRemoveShipmentCmd(user.id, f.shipment)
          shipmentsService.removeShipment(cmd) mustFail "Unauthorized"
        }
      }

    }

  }

}
