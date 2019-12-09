package org.biobank.services.centres

import org.biobank.fixtures._
import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.users._
import org.biobank.services.{FilterString, PagedQuery, SortString}
import org.biobank.services.users.UserServiceFixtures
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.biobank.infrastructure.commands.ContainerTypeCommands._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Primarily these are tests that exercise the User Access aspect of ContainerTypesService.
 */
class StorageContainerTypesServiceSpec
    extends CommonContainerTypeControllerSpec[StorageContainerType, StorageContainerTypeAccessFixture] {
  import org.biobank.TestUtils._

  describe("ContainerTypesService (Storage Container Type)") {

    describe("when adding a container type") {

      it("users can access") {
        val f = fixtureAddAllButContainerType
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddStorageContainerTypeCmd(sessionUserId = user.id.id,
                                       name          = f.containerType.name,
                                       description   = f.containerType.description,
                                       centreId      = f.containerType.centreId.id,
                                       schemaId      = f.containerType.schemaId.id,
                                       shared        = f.containerType.shared)
          containerTypeRepository.removeAll
          containerTypesService.processCommand(cmd).mustSucceed { c =>
            c.name must be(f.containerType.name)
          }
        }
      }

      it("users cannot access") {
        val f = fixtureAddAllButContainerType
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddStorageContainerTypeCmd(sessionUserId = user.id.id,
                                       name          = f.containerType.name,
                                       description   = f.containerType.description,
                                       centreId      = f.containerType.centreId.id,
                                       schemaId      = f.containerType.schemaId.id,
                                       shared        = f.containerType.shared)
          containerTypesService.processCommand(cmd) mustFail "Unauthorized"
        }
      }
    }

  }

  protected def fixture(): StorageContainerTypeAccessFixture = new StorageContainerTypeAccessFixture(factory)

}

/**
 * Primarily these are tests that exercise the User Access aspect of ContainerTypesService.
 */
class SpecimenContainerTypesServiceSpec
    extends CommonContainerTypeControllerSpec[SpecimenContainerType, SpecimenContainerTypeAccessFixture] {
  import org.biobank.TestUtils._

  describe("ContainerTypesService (Specimen Container Type)") {

    describe("when adding a container type") {

      it("users can access") {
        val f = fixtureAddAllButContainerType
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddStorageContainerTypeCmd(sessionUserId = user.id.id,
                                       name          = f.containerType.name,
                                       description   = f.containerType.description,
                                       centreId      = f.containerType.centreId.id,
                                       schemaId      = f.containerType.schemaId.id,
                                       shared        = f.containerType.shared)
          containerTypeRepository.removeAll
          containerTypesService.processCommand(cmd).mustSucceed { c =>
            c.name must be(f.containerType.name)
          }
        }
      }

      it("users cannot access") {
        val f = fixtureAddAllButContainerType
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddStorageContainerTypeCmd(sessionUserId = user.id.id,
                                       name          = f.containerType.name,
                                       description   = f.containerType.description,
                                       centreId      = f.containerType.centreId.id,
                                       schemaId      = f.containerType.schemaId.id,
                                       shared        = f.containerType.shared)
          containerTypesService.processCommand(cmd) mustFail "Unauthorized"
        }
      }
    }

  }

  protected def fixture(): SpecimenContainerTypeAccessFixture =
    new SpecimenContainerTypeAccessFixture(factory)

}

trait CommonContainerTypeControllerSpec[
    C <: ContainerType,
    F <: UsersWithCentreAccessFixture with ContainerTypeAccessFixture[C]]
    extends ProcessorTestFixture with UserServiceFixtures with ScalaFutures {

  import org.biobank.TestUtils._

  protected def fixture(): F

  protected def fixtureAddAll: F = {
    val f = fixture
    addAllToRepository(f.allEntities)
    f
  }

  protected def fixtureAddAllButContainerType(): F = {
    val f = fixture
    addAllToRepository(f.allEntitiesButContainerType)
    f
  }

  protected val nameGenerator             = new NameGenerator(this.getClass)
  protected val accessItemRepository      = app.injector.instanceOf[AccessItemRepository]
  protected val membershipRepository      = app.injector.instanceOf[MembershipRepository]
  protected val userRepository            = app.injector.instanceOf[UserRepository]
  protected val centreRepository          = app.injector.instanceOf[CentreRepository]
  protected val containerTypeRepository   = app.injector.instanceOf[ContainerTypeRepository]
  protected val containerSchemaRepository = app.injector.instanceOf[ContainerSchemaRepository]
  protected val containerTypesService     = app.injector.instanceOf[ContainerTypesService]

  protected def updateCommandsTable(sessionUserId: UserId, containerType: C) =
    Table("containerType update commands",
          UpdateContainerTypeNameCmd(sessionUserId          = sessionUserId.id,
                                     id                     = containerType.id.id,
                                     expectedVersion        = containerType.version,
                                     name                   = nameGenerator.next[String]),
          UpdateContainerTypeDescriptionCmd(sessionUserId   = sessionUserId.id,
                                            id              = containerType.id.id,
                                            expectedVersion = containerType.version,
                                            description     = Some(nameGenerator.next[String])),
          UpdateContainerTypeCentreCmd(sessionUserId        = sessionUserId.id,
                                       id                   = containerType.id.id,
                                       expectedVersion      = containerType.version,
                                       centreId             = containerType.centreId.id),
          UpdateContainerTypeSchemaCmd(sessionUserId        = sessionUserId.id,
                                       id                   = containerType.id.id,
                                       expectedVersion      = containerType.version,
                                       schemaId             = containerType.schemaId.id),
          UpdateContainerTypeSharedCmd(sessionUserId        = sessionUserId.id,
                                       id                   = containerType.id.id,
                                       expectedVersion      = containerType.version,
                                       shared               = !containerType.shared),
          UpdateContainerTypeEnabledCmd(sessionUserId       = sessionUserId.id,
                                        id                  = containerType.id.id,
                                        expectedVersion     = containerType.version,
                                        enabled             = !containerType.enabled))

  override def beforeEach() {
    super.beforeEach()
    removeUsersFromRepository
    restoreRoles
    containerTypeRepository.removeAll
  }

  describe("ContainerTypesService (common)") {

    describe("when searching containerTypes") {

      it("users can access") {
        val f = fixtureAddAll
        persistRoles(f)
        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          containerTypesService
            .search(user.id, f.centre.id, query)
            .mustSucceed { pagedResults =>
              pagedResults.items.length must be > 0
            }
        }
      }

      it("users cannot access") {
        val f = fixtureAddAll
        persistRoles(f)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        info("no membership user")
        containerTypesService
          .search(f.noMembershipUser.id, f.centre.id, query).mustFail("Unauthorized")

        info("no permission user")
        containerTypesService
          .search(f.noCentrePermissionUser.id, f.centre.id, query).mustFail("Unauthorized")
      }

    }

    describe("when getting a containerType") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          containerTypesService.getBySlug(user.id, f.containerType.slug).mustSucceed { result =>
            result.id must be(f.containerType.id.id)
          }
        }
      }

      it("users cannot access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        info("no membership user")
        containerTypesService
          .getBySlug(f.noMembershipUser.id, f.containerType.slug).mustFail("Unauthorized")

        info("no permission user")
        containerTypesService
          .getBySlug(f.noCentrePermissionUser.id, f.containerType.slug).mustFail("Unauthorized")
      }

    }

    describe("update a containerType") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(updateCommandsTable(user.id, f.containerType)) { cmd =>
            containerTypeRepository.put(f.containerType) // restore the containerType to it's previous state
            containerTypesService.processCommand(cmd).mustSucceed { c =>
              c.id must be(f.containerType.id.id)
            }
          }
        }
      }

      it("users cannot update") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          forAll(updateCommandsTable(user.id, f.containerType)) { cmd =>
            containerTypesService.processCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

    describe("centre membership") {

      it("user has access to all containerTypes corresponding his membership") {
        val f = fixtureAddAll
        persistRoles(f)

        val sibling = f.createSibling(factory) // should show up in results
        addToRepository(sibling)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 10)

        containerTypesService.search(f.allCentresAdminUser.id, f.centre.id, query).mustSucceed { reply =>
          reply.items.length must be > 1
          val containerTypeIds = reply.items.map(c => c.id).sorted
          containerTypeIds must equal(List(f.containerType.id.id, sibling.id.id).sorted)
        }
      }

      it("user does not have access to containerType if centre not in membership") {
        val f = fixtureAddAll
        persistRoles(f)

        // remove all centres from membership
        val noCentresMembership = f.centreOnlyMembership.copy(
          centreData = f.centreOnlyMembership.centreData.copy(ids = Set.empty[CentreId])
        )
        addToRepository(noCentresMembership)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        containerTypesService.search(f.centreUser.id, f.centre.id, query).mustFail("Unauthorized")
      }

    }

  }

  override protected def addToRepository[E <: ConcurrencySafeEntity[_]](entity: E): Unit =
    entity match {
      case u: User            => userRepository.put(u)
      case i: AccessItem      => accessItemRepository.put(i)
      case m: Membership      => membershipRepository.put(m)
      case c: Centre          => centreRepository.put(c)
      case c: ContainerType   => containerTypeRepository.put(c)
      case c: ContainerSchema => containerSchemaRepository.put(c)
      case e => fail(s"invalid entity: $e")
    }

  protected def addAllToRepository(entities: Set[ConcurrencySafeEntity[_]]): Unit =
    entities.foreach(addToRepository)

  protected def persistRoles(f: F): Unit = {
    addUserToCentreAdminRole(f.allCentresAdminUser)
    addUserToCentreAdminRole(f.centreOnlyAdminUser)
    addUserToRole(f.centreUser, RoleId.CentreUser)
    addUserToRole(f.noMembershipUser, RoleId.CentreUser)
  }

  protected def addUserToCentreAdminRole(user: User): Unit =
    addUserToRole(user, RoleId.CentreAdministrator)

}

trait ContainerTypeAccessFixture[T <: ContainerType] extends UserCentreAccessFixture {

  val containerTypeEntities: ContainerTypeFixture[T]
  val containerType:         T

  override def allEntities: Set[ConcurrencySafeEntity[_]] =
    containerTypeEntities.allEntities ++ super.allEntities

  def allEntitiesButContainerType: Set[ConcurrencySafeEntity[_]] =
    containerTypeEntities.allEntitiesButContainerType ++ super.allEntities

  def createSibling(factory: Factory) = containerTypeEntities.createSibling(factory)

}

class StorageContainerTypeAccessFixture(factory: Factory)
    extends UsersWithCentreAccessFixture(factory) with ContainerTypeAccessFixture[StorageContainerType] {

  val containerTypeEntities = StorageContainerTypeFixture(factory, centre)
  val containerType: StorageContainerType = containerTypeEntities.containerType

}

class SpecimenContainerTypeAccessFixture(factory: Factory)
    extends UsersWithCentreAccessFixture(factory) with ContainerTypeAccessFixture[SpecimenContainerType] {

  val containerTypeEntities = SpecimenContainerTypeFixture(factory, centre)
  val containerType: SpecimenContainerType = containerTypeEntities.containerType

}
