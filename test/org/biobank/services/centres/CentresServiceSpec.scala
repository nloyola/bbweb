package org.biobank.services.centres

import org.biobank.fixtures._
import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.studies._
import org.biobank.domain.users._
import org.biobank.services.{FilterString, PagedQuery, SortString}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.TableDrivenPropertyChecks._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Primarily these are tests that exercise the User Access aspect of CentresService.
 */
class CentresServiceSpec extends CentresServiceFixtures with ScalaFutures {

  import org.biobank.TestUtils._
  import org.biobank.infrastructure.commands.CentreCommands._

  class CentresOfAllStatesFixure(factory: Factory) extends UsersWithCentreAccessFixture(factory) {
    val disabledCentre = centre
    val enabledCentre  = factory.createEnabledCentre

    override def allEntities(): Set[ConcurrencySafeEntity[_]] =
      Set(disabledCentre, enabledCentre) ++ super.allEntities

  }

  protected def createCentresOfAllStatesFixure(): CentresOfAllStatesFixure = {
    val f = new CentresOfAllStatesFixure(factory)
    f.allEntities.foreach(addToRepository)
    persistRoles(f)
    addToRepository(
      f.centreOnlyMembership.copy(
        centreData =
          f.centreOnlyMembership.centreData.copy(ids = Set(f.disabledCentre.id, f.enabledCentre.id))
      )
    )
    f
  }

  protected val nameGenerator = new NameGenerator(this.getClass)

  protected val accessItemRepository = app.injector.instanceOf[AccessItemRepository]

  protected val membershipRepository = app.injector.instanceOf[MembershipRepository]

  protected val userRepository = app.injector.instanceOf[UserRepository]

  protected val centreRepository = app.injector.instanceOf[CentreRepository]

  protected val studyRepository = app.injector.instanceOf[StudyRepository]

  protected val centresService = app.injector.instanceOf[CentresService]

  private def updateCommandsTable(sessionUserId: UserId, centre: Centre, location: Location, study: Study) =
    Table("centre update commands",
          UpdateCentreNameCmd(sessionUserId          = sessionUserId.id,
                              id                     = centre.id.id,
                              expectedVersion        = centre.version,
                              name                   = nameGenerator.next[String]),
          UpdateCentreDescriptionCmd(sessionUserId   = sessionUserId.id,
                                     id              = centre.id.id,
                                     expectedVersion = centre.version,
                                     description     = Some(nameGenerator.next[String])),
          AddCentreLocationCmd(sessionUserId         = sessionUserId.id,
                               id                    = centre.id.id,
                               expectedVersion       = centre.version,
                               name                  = location.name,
                               street                = location.street,
                               city                  = location.city,
                               province              = location.province,
                               postalCode            = location.postalCode,
                               poBoxNumber           = location.poBoxNumber,
                               countryIsoCode        = location.countryIsoCode),
          RemoveCentreLocationCmd(sessionUserId      = sessionUserId.id,
                                  id                 = centre.id.id,
                                  expectedVersion    = centre.version,
                                  locationId         = location.id.id),
          AddStudyToCentreCmd(sessionUserId          = sessionUserId.id,
                              id                     = centre.id.id,
                              expectedVersion        = centre.version,
                              studyId                = study.id.id),
          RemoveStudyFromCentreCmd(sessionUserId     = sessionUserId.id,
                                   id                = centre.id.id,
                                   expectedVersion   = centre.version,
                                   studyId           = study.id.id))

  private def stateChangeCommandsTable(
      sessionUserId:  UserId,
      disabledCentre: DisabledCentre,
      enabledCentre:  EnabledCentre
    ) =
    Table("user sate change commands",
          EnableCentreCmd(sessionUserId    = sessionUserId.id,
                          id               = disabledCentre.id.id,
                          expectedVersion  = disabledCentre.version),
          DisableCentreCmd(sessionUserId   = sessionUserId.id,
                           id              = enabledCentre.id.id,
                           expectedVersion = enabledCentre.version))

  override def beforeEach() = {
    super.beforeEach()
    removeUsersFromRepository
    restoreRoles
    centreRepository.removeAll
  }

  describe("when getting centre counts") {

    it("users can access") {
      val f = createFixture
      forAll(f.usersCanReadTable) { (user, label) =>
        info(label)
        centresService.getCentresCount(user.id) mustSucceed { count =>
          count must be(1)
        }
      }
    }

    it("users cannot access") {
      val f = createFixture

      info("no membership user")
      centresService.getCentresCount(f.noMembershipUser.id) mustSucceed { count =>
        count must be(0)
      }

      info("no permission user")
      centresService.getCentresCount(f.noCentrePermissionUser.id) mustFail "Unauthorized"
    }

  }

  describe("when getting centre counts by status") {

    it("users can access") {
      val f = createFixture
      forAll(f.usersCanReadTable) { (user, label) =>
        info(label)
        centresService.getCountsByStatus(user.id) mustSucceed { counts =>
          counts.total must be(1)
        }
      }
    }

    it("users cannot access") {
      val f = createFixture
      info("no membership user")
      centresService.getCountsByStatus(f.noMembershipUser.id) mustSucceed { counts =>
        counts.total must be(0)
      }

      info("no permission user")
      centresService.getCountsByStatus(f.noCentrePermissionUser.id) mustFail "Unauthorized"
    }

  }

  describe("when getting centres") {

    it("users can access") {
      val f     = createFixture
      val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
      forAll(f.usersCanReadTable) { (user, label) =>
        info(label)
        centresService.getCentres(user.id, query).mustSucceed { pagedResults =>
          pagedResults.items must have length (1)
        }
      }
    }

    it("users cannot access") {
      val f     = createFixture
      val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
      info("no membership user")
      centresService.getCentres(f.noMembershipUser.id, query).mustSucceed { pagedResults =>
        pagedResults.items must have length (0)
      }

      info("no permission user")
      centresService.getCentres(f.noCentrePermissionUser.id, query).mustFail("Unauthorized")
    }

  }

  describe("when getting a centre") {

    it("users can access") {
      val f = createFixture
      forAll(f.usersCanReadTable) { (user, label) =>
        info(label)
        centresService.getCentre(user.id, f.centre.id) mustSucceed { result =>
          result.id must be(f.centre.id)
        }
      }
    }

    it("users cannot access") {
      val f = createFixture

      info("no membership user")
      centresService.getCentre(f.noMembershipUser.id, f.centre.id) mustFail "Unauthorized"

      info("no permission user")
      centresService.getCentre(f.noCentrePermissionUser.id, f.centre.id) mustFail "Unauthorized"
    }

  }

  describe("search locations") {

    it("users can access") {
      val f = createFixture
      forAll(f.usersCanReadTable) { (user, label) =>
        info(label)
        val cmd = SearchCentreLocationsCmd(sessionUserId = user.id.id, filter = "", limit = 10)
        centresService.searchLocations(cmd).mustSucceed { centres =>
          centres must have size (1)
        }
      }
    }

    it("users cannot access") {
      val f   = createFixture
      var cmd = SearchCentreLocationsCmd(sessionUserId = f.noMembershipUser.id.id, filter = "", limit = 10)

      info("no membership user")
      centresService.searchLocations(cmd).mustSucceed { centres =>
        centres must have size (0)
      }

      cmd = SearchCentreLocationsCmd(sessionUserId = f.noCentrePermissionUser.id.id, filter = "", limit = 10)
      info("no permission user")
      centresService.searchLocations(cmd) mustFail "Unauthorized"
    }

  }

  describe("when adding a centre") {

    it("users can access") {
      val f = createFixture

      forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
        val cmd =
          AddCentreCmd(sessionUserId = user.id.id, name = f.centre.name, description = f.centre.description)
        centreRepository.removeAll
        centresService.processCommand(cmd) mustSucceed { s =>
          s.name must be(f.centre.name)
        }
      }
    }

    it("users cannot access") {
      val f = createFixture

      forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
        val cmd =
          AddCentreCmd(sessionUserId = user.id.id, name = f.centre.name, description = f.centre.description)
        centresService.processCommand(cmd) mustFail "Unauthorized"
      }
    }

  }

  describe("update a centre") {

    it("users can access") {
      val f     = createFixture
      val study = factory.createDisabledStudy
      studyRepository.put(study)

      forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
        info(label)
        forAll(updateCommandsTable(user.id, f.centre, f.location, study)) { cmd =>
          val centre = cmd match {
            case _: AddCentreLocationCmd     => f.centre.copy(locations = Set.empty[Location])
            case _: RemoveStudyFromCentreCmd => f.centre.copy(studyIds  = Set(study.id))
            case _ => f.centre
          }

          centreRepository.put(centre) // restore the centre to it's previous state
          centresService.processCommand(cmd).mustSucceed { c =>
            c.id must be(centre.id.id)
          }
        }
      }
    }

    it("users cannot update") {
      val f     = createFixture
      val study = factory.createDisabledStudy
      studyRepository.put(study)

      forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
        forAll(updateCommandsTable(user.id, f.centre, f.location, study)) { cmd =>
          centresService.processCommand(cmd) mustFail "Unauthorized"
        }
      }
    }

  }

  describe("change a centre's state") {

    it("users can access") {
      val f = createCentresOfAllStatesFixure
      forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
        info(label)
        forAll(stateChangeCommandsTable(user.id, f.disabledCentre, f.enabledCentre)) { cmd =>
          Set(f.disabledCentre, f.enabledCentre).foreach(addToRepository)
          centresService.processCommand(cmd).mustSucceed { c =>
            c.id must be(cmd.id)
          }
        }
      }
    }

    it("users cannot update") {
      val f = createCentresOfAllStatesFixure
      forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
        info(label)
        forAll(stateChangeCommandsTable(user.id, f.disabledCentre, f.enabledCentre)) { cmd =>
          centresService.processCommand(cmd) mustFail "Unauthorized"
        }
      }
    }
  }

  describe("centres membership") {

    it("user has access to all centres corresponding his membership") {
      val f            = createFixture
      val secondCentre = factory.createDisabledCentre // should show up in results
      addToRepository(secondCentre)

      val query = PagedQuery(new FilterString(""), new SortString(""), 0, 10)
      centresService.getCentres(f.allCentresAdminUser.id, query).mustSucceed { reply =>
        reply.items.length must be > 1
        val centreIds = reply.items.map(c => c.id).sorted
        centreIds must equal(List(f.centre.id.id, secondCentre.id.id).sorted)
      }
    }

    it("user has access only to centres corresponding his membership") {
      val f            = createFixture
      val secondCentre = factory.createDisabledCentre // should show up in results
      addToRepository(secondCentre)

      val query = PagedQuery(new FilterString(""), new SortString(""), 0, 10)
      centresService.getCentres(f.centreOnlyAdminUser.id, query).mustSucceed { reply =>
        reply.items must have size (1)
        reply.items.map(c => c.id) must contain(f.centre.id.id)
      }
    }

    it("user does not have access to centre if not in membership") {
      val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
      val f     = createFixture

      // remove all studies from membership
      val noCentresMembership = f.centreOnlyMembership.copy(
        centreData = f.centreOnlyMembership.centreData.copy(ids = Set.empty[CentreId])
      )
      addToRepository(noCentresMembership)

      // should not show up in results
      val secondStudy = factory.createDisabledStudy
      addToRepository(secondStudy)

      centresService.getCentres(f.centreUser.id, query).mustSucceed { reply =>
        reply.items must have size (0)
      }
    }

  }

}
