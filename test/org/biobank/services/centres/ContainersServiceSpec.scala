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
import org.biobank.infrastructure.commands.ContainerCommands._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Primarily these are tests that exercise the User Access aspect of ContainersService.
 */
class RootContainersServiceSpec
    extends CommonContainerControllerSpec[RootContainer, StorageContainerType, RootContainerAccessFixture] {
  import org.biobank.TestUtils._

  describe("ContainersService (root container)") {

    describe("when searching containers") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)
        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          containersService
            .search(user.id, f.centre.id, query)
            .mustSucceed { pagedResults =>
              pagedResults.items.length must be > 0
            }
        }
      }

      it("users cannot access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)
        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        info("no membership user")
        containersService
          .search(f.noMembershipUser.id, f.centre.id, query).mustFail("Unauthorized")

        info("no permission user")
        containersService
          .search(f.noCentrePermissionUser.id, f.centre.id, query).mustFail("Unauthorized")
      }

    }

    describe("when adding a container") {

      it("users can access") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddRootContainerCmd(sessionUserId   = user.id.id,
                                inventoryId     = f.container.inventoryId,
                                label           = f.container.label,
                                centreId        = f.container.centreId.id,
                                locationId      = f.container.locationId.id,
                                temperature     = f.container.temperature,
                                containerTypeId = f.container.containerTypeId.id)
          containerRepository.removeAll
          containersService.processCommand(cmd).mustSucceed { c =>
            c.inventoryId must be(f.container.inventoryId)
          }
        }
      }

      it("users cannot access") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddRootContainerCmd(sessionUserId   = user.id.id,
                                inventoryId     = f.container.inventoryId,
                                label           = f.container.label,
                                centreId        = f.container.centreId.id,
                                locationId      = f.container.locationId.id,
                                temperature     = f.container.temperature,
                                containerTypeId = f.container.containerTypeId.id)
          containersService.processCommand(cmd) mustFail "Unauthorized"
        }
      }
    }

    describe("centre membership") {

      it("user has access to all containers corresponding his membership") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        val siblingContainer = f.createSiblingContainer(factory) // should show up in results
        addToRepository(siblingContainer)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 10)

        containersService.search(f.allCentresAdminUser.id, f.centre.id, query).mustSucceed { reply =>
          reply.items must have size (2)
          val containerIds = reply.items.map(c => c.id).sorted
          containerIds must equal(List(f.container.id.id, siblingContainer.id.id).sorted)
        }
      }

      it("user does not have access to container if centre not in membership") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        // remove all centres from membership
        val noCentresMembership = f.centreOnlyMembership.copy(
          centreData = f.centreOnlyMembership.centreData.copy(ids = Set.empty[CentreId])
        )
        addToRepository(noCentresMembership)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        containersService.search(f.centreUser.id, f.centre.id, query).mustFail("Unauthorized")
      }

    }

  }

  override protected def updateCommandsTable(sessionUserId: UserId, container: RootContainer) = {
    val constraints = factory.createContainerConstraints
    super.updateCommandsTable(sessionUserId, container) ++
      Table("container update commands",
            UpdateContainerLabelCmd(sessionUserId            = sessionUserId.id,
                                    id                       = container.id.id,
                                    expectedVersion          = container.version,
                                    label                    = nameGenerator.next[String]),
            UpdateContainerEnabledCmd(sessionUserId          = sessionUserId.id,
                                      id                     = container.id.id,
                                      expectedVersion        = container.version,
                                      enabled                = !container.enabled),
            UpdateContainerCentreLocationCmd(sessionUserId   = sessionUserId.id,
                                             id              = container.id.id,
                                             expectedVersion = container.version,
                                             centreId        = container.centreId.id,
                                             locationId      = container.locationId.id),
            UpdateContainerTemperatureCmd(sessionUserId      = sessionUserId.id,
                                          id                 = container.id.id,
                                          expectedVersion    = container.version,
                                          temperature        = container.temperature),
            UpdateContainerConstraintsCmd(sessionUserId      = sessionUserId.id,
                                          id                 = container.id.id,
                                          expectedVersion    = container.version,
                                          name               = constraints.name,
                                          description        = constraints.description,
                                          anatomicalSources  = constraints.anatomicalSources,
                                          preservationTypes  = constraints.preservationTypes,
                                          specimenTypes      = constraints.specimenTypes))
  }

  protected def fixture(): RootContainerAccessFixture = new RootContainerAccessFixture(factory)

}

class StorageContainersServiceSpec
    extends ChildContainerControllerSpec[StorageContainer,
                                         StorageContainerType,
                                         StorageContainerAccessFixture] {
  val newLabel: String = RootContainerFixture.labels.toSeq(1)

  protected def fixture(): StorageContainerAccessFixture = new StorageContainerAccessFixture(factory)

}

class SpecimenContainersServiceSpec
    extends CommonContainerControllerSpec[SpecimenContainer,
                                          SpecimenContainerType,
                                          SpecimenContainerAccessFixture] {
  val newLabel: String = StorageContainerFixture.labels.toSeq(1)

  protected def fixture(): SpecimenContainerAccessFixture = new SpecimenContainerAccessFixture(factory)

}

trait ChildContainerControllerSpec[
    C <: ChildContainer,
    T <: ContainerType,
    F <: UsersWithCentreAccessFixture with ContainerAccessFixture[C, T]]
    extends CommonContainerControllerSpec[C, T, F] {

  import org.biobank.TestUtils._

  val newLabel: String

  protected def fixture(): F

  describe("ContainersService (child container)") {
    describe("when adding a container") {

      it("users can access") {
        val f = fixture
        f.allEntitiesButContainer.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          val cmd =
            AddStorageContainerCmd(sessionUserId   = user.id.id,
                                   inventoryId     = f.container.inventoryId,
                                   label           = f.container.schemaLabel.label,
                                   containerTypeId = f.container.containerTypeId.id,
                                   parentId        = f.container.parentId.id)
          containersService.processCommand(cmd).mustSucceed { c =>
            c.inventoryId must be(f.container.inventoryId)

            val repoContainer = containerRepository.getByKey(ContainerId(c.id)).toOption.value
            containerRepository.remove(repoContainer)
          }
        }
      }
    }
  }

  override protected def updateCommandsTable(sessionUserId: UserId, container: C) = {
    super.updateCommandsTable(sessionUserId, container) ++
      Table("container update commands",
            UpdateContainerLabelCmd(sessionUserId      = sessionUserId.id,
                                    id                 = container.id.id,
                                    expectedVersion    = container.version,
                                    label              = newLabel),
            UpdateContainerPositionCmd(sessionUserId   = sessionUserId.id,
                                       id              = container.id.id,
                                       expectedVersion = container.version,
                                       parentId        = container.parentId.id,
                                       label           = newLabel))
  }
}

trait CommonContainerControllerSpec[
    C <: Container,
    T <: ContainerType,
    F <: UsersWithCentreAccessFixture with ContainerAccessFixture[C, T]]
    extends ProcessorTestFixture with UserServiceFixtures with ScalaFutures {

  import org.biobank.TestUtils._
  // import org.biobank.infrastructure.commands.ContainerCommands._

  protected def fixture(): F

  protected val nameGenerator             = new NameGenerator(this.getClass)
  protected val accessItemRepository      = app.injector.instanceOf[AccessItemRepository]
  protected val membershipRepository      = app.injector.instanceOf[MembershipRepository]
  protected val userRepository            = app.injector.instanceOf[UserRepository]
  protected val centreRepository          = app.injector.instanceOf[CentreRepository]
  protected val containerRepository       = app.injector.instanceOf[ContainerRepository]
  protected val containerTypeRepository   = app.injector.instanceOf[ContainerTypeRepository]
  protected val containerSchemaRepository = app.injector.instanceOf[ContainerSchemaRepository]
  protected val containersService         = app.injector.instanceOf[ContainersService]

  protected def updateCommandsTable(sessionUserId: UserId, container: C) =
    Table("container update commands",
          UpdateContainerInventoryIdCmd(sessionUserId     = sessionUserId.id,
                                        id                = container.id.id,
                                        expectedVersion   = container.version,
                                        inventoryId       = nameGenerator.next[String]),
          UpdateContainerContainerTypeCmd(sessionUserId   = sessionUserId.id,
                                          id              = container.id.id,
                                          expectedVersion = container.version,
                                          containerTypeId = container.containerTypeId.id))

  override def beforeEach() {
    super.beforeEach()
    removeUsersFromRepository
    restoreRoles
    containerRepository.removeAll
  }

  describe("ContainersService (common)") {

    describe("when getting a container") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          containersService.getBySlug(user.id, f.container.slug).mustSucceed { result =>
            result.id must be(f.container.id.id)
          }
        }
      }

      it("users cannot access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        info("no membership user")
        containersService
          .getBySlug(f.noMembershipUser.id, f.container.slug).mustFail("Unauthorized")

        info("no permission user")
        containersService
          .getBySlug(f.noCentrePermissionUser.id, f.container.slug).mustFail("Unauthorized")
      }

    }

    describe("update a container") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(updateCommandsTable(user.id, f.container)) { cmd =>
            containerRepository.put(f.container) // restore the container to it's previous state
            containersService.processCommand(cmd).mustSucceed { c =>
              c.id must be(f.container.id.id)
            }
          }
        }
      }

      it("users cannot update") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          forAll(updateCommandsTable(user.id, f.container)) { cmd =>
            containersService.processCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

  }

  override protected def addToRepository[E <: ConcurrencySafeEntity[_]](entity: E): Unit =
    entity match {
      case u: User            => userRepository.put(u)
      case i: AccessItem      => accessItemRepository.put(i)
      case m: Membership      => membershipRepository.put(m)
      case c: Centre          => centreRepository.put(c)
      case c: Container       => containerRepository.put(c)
      case c: ContainerType   => containerTypeRepository.put(c)
      case c: ContainerSchema => containerSchemaRepository.put(c)
      case e => fail(s"invalid entity: $e")
    }

  protected def persistRoles(f: F): Unit = {
    addUserToCentreAdminRole(f.allCentresAdminUser)
    addUserToCentreAdminRole(f.centreOnlyAdminUser)
    addUserToRole(f.centreUser, RoleId.CentreUser)
    addUserToRole(f.noMembershipUser, RoleId.CentreUser)
  }

  protected def addUserToCentreAdminRole(user: User): Unit =
    addUserToRole(user, RoleId.CentreAdministrator)

}

trait ContainerAccessFixture[C <: Container, T <: ContainerType] extends UserCentreAccessFixture {

  val containerEntities: ContainerFixture[C, T]
  val container:         C

  override def allEntities: Set[ConcurrencySafeEntity[_]] =
    containerEntities.allEntities ++ super.allEntities

  def allEntitiesButContainer: Set[ConcurrencySafeEntity[_]] =
    containerEntities.allEntitiesButContainer ++ super.allEntities

  def createSiblingContainer(factory: Factory) = containerEntities.createSiblingContainer(factory)

}

trait ChildContainerAccessFixture[C <: ChildContainer, T <: ContainerType]
    extends ContainerAccessFixture[C, T] with UserCentreAccessFixture {}

class RootContainerAccessFixture(factory: Factory)
    extends UsersWithCentreAccessFixture(factory)
    with ContainerAccessFixture[RootContainer, StorageContainerType] {

  val containerEntities = RootContainerFixture(factory, centre)
  val container: RootContainer = containerEntities.container

}

class StorageContainerAccessFixture(factory: Factory)
    extends UsersWithCentreAccessFixture(factory)
    with ChildContainerAccessFixture[StorageContainer, StorageContainerType] {

  val containerEntities = StorageContainerFixture(factory, centre)
  val container: StorageContainer = containerEntities.container

}

class SpecimenContainerAccessFixture(factory: Factory)
    extends UsersWithCentreAccessFixture(factory)
    with ChildContainerAccessFixture[SpecimenContainer, SpecimenContainerType] {

  val containerEntities = SpecimenContainerFixture(factory, centre)
  val container: SpecimenContainer = containerEntities.container

}
