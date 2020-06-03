package org.biobank.services.centres

import org.biobank.fixtures._
import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.users._
import org.biobank.services.{FilterString, PagedQuery, SortString}
import org.biobank.services.users.UserServiceFixtures
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.biobank.infrastructure.commands.ContainerSchemaCommands._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Primarily these are tests that exercise the User Access aspect of ContainerSchemasService.
 */
class ContainerSchemasServiceSpec extends ProcessorTestFixture with UserServiceFixtures {
  import org.biobank.TestUtils._
  import org.scalatest.matchers.must.Matchers._

  override def beforeEach() = {
    super.beforeEach()
    removeUsersFromRepository
    restoreRoles
    schemaRepository.removeAll
  }

  describe("ContainerSchemasService (Storage Container Type)") {

    describe("when adding a container type") {

      it("users can access") {
        val f = fixtureAddAllButContainerSchema
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          val cmd = containerToAddCommand(user, f.schema)
          schemaRepository.removeAll
          schemasService.processCommand(cmd).mustSucceed { c =>
            c.name must be(f.schema.name)
          }
        }
      }

      it("users cannot access") {
        val f = fixtureAddAllButContainerSchema
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          val cmd = containerToAddCommand(user, f.schema)
          schemasService.processCommand(cmd) mustFail "Unauthorized"
        }
      }
    }

  }

  describe("ContainerSchemasService (common)") {

    describe("when searching containerSchemas") {

      it("users can access") {
        val f = fixtureAddAll
        persistRoles(f)
        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          schemasService
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
        schemasService
          .search(f.noMembershipUser.id, f.centre.id, query).mustFail("Unauthorized")

        info("no permission user")
        schemasService
          .search(f.noCentrePermissionUser.id, f.centre.id, query).mustFail("Unauthorized")
      }

    }

    describe("when getting a containerSchema") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          schemasService.getBySlug(user.id, f.schema.slug).mustSucceed { result =>
            result.id must be(f.schema.id)
          }
        }
      }

      it("users cannot access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        info("no membership user")
        schemasService.getBySlug(f.noMembershipUser.id, f.schema.slug).mustFail("Unauthorized")

        info("no permission user")
        schemasService
          .getBySlug(f.noCentrePermissionUser.id, f.schema.slug).mustFail("Unauthorized")
      }

    }

    describe("update a containerSchema") {

      it("users can access") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(updateCommandsTable(user.id, f.schema)) { cmd =>
            schemaRepository.put(f.schema) // restore the containerSchema to it's previous state
            schemasService.processCommand(cmd).mustSucceed { c =>
              c.id must be(f.schema.id)
            }
          }
        }
      }

      it("users cannot update") {
        val f = fixture
        f.allEntities.foreach(addToRepository)
        persistRoles(f)

        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          forAll(updateCommandsTable(user.id, f.schema)) { cmd =>
            schemasService.processCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

    describe("centre membership") {

      it("user has access to all container schemas corresponding his membership") {
        val f = fixtureAddAll
        persistRoles(f)

        val sibling = f.createSibling(factory) // should show up in results
        addToRepository(sibling)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 10)

        schemasService.search(f.allCentresAdminUser.id, f.centre.id, query).mustSucceed { reply =>
          reply.items.length must be > 1
          val containerSchemaIds = reply.items.map(c => c.id.id).sorted
          containerSchemaIds must equal(List(f.schema.id.id, sibling.id.id).sorted)
        }
      }

      it("user does not have access to container schema if centre not in membership") {
        val f = fixtureAddAll
        persistRoles(f)

        // remove all centres from membership
        val noCentresMembership = f.centreOnlyMembership.copy(
          centreData = f.centreOnlyMembership.centreData.copy(ids = Set.empty[CentreId])
        )
        addToRepository(noCentresMembership)

        val query = PagedQuery(new FilterString(""), new SortString(""), 0, 1)
        schemasService
          .search(f.centreUser.id, f.centre.id, query).mustFail("Unauthorized")
      }

    }

  }

  protected val nameGenerator        = new NameGenerator(this.getClass)
  protected val accessItemRepository = app.injector.instanceOf[AccessItemRepository]
  protected val membershipRepository = app.injector.instanceOf[MembershipRepository]
  protected val userRepository       = app.injector.instanceOf[UserRepository]
  protected val centreRepository     = app.injector.instanceOf[CentreRepository]
  protected val schemaRepository     = app.injector.instanceOf[ContainerSchemaRepository]
  protected val schemasService       = app.injector.instanceOf[ContainerSchemasService]

  protected def fixture() = new ContainerSchemaAccessFixture(factory)

  protected def fixtureAddAll: ContainerSchemaAccessFixture = {
    val f = fixture
    addAllToRepository(f.allEntities)
    f
  }

  protected def fixtureAddAllButContainerSchema(): ContainerSchemaAccessFixture = {
    val f = fixture
    addAllToRepository(f.allEntitiesButSchema)
    f
  }

  protected def containerToAddCommand(user: User, schema: ContainerSchema) =
    AddContainerSchemaCmd(sessionUserId = user.id.id,
                          name          = schema.name,
                          description   = schema.description,
                          shared        = schema.shared,
                          centreId      = schema.centreId.id,
                          labels        = schema.labels.toList)

  protected def updateCommandsTable(sessionUserId: UserId, containerSchema: ContainerSchema) =
    Table("containerSchema update commands",
          UpdateContainerSchemaNameCmd(sessionUserId          = sessionUserId.id,
                                       id                     = containerSchema.id.id,
                                       expectedVersion        = containerSchema.version,
                                       name                   = nameGenerator.next[String]),
          UpdateContainerSchemaDescriptionCmd(sessionUserId   = sessionUserId.id,
                                              id              = containerSchema.id.id,
                                              expectedVersion = containerSchema.version,
                                              description     = Some(nameGenerator.next[String])),
          UpdateContainerSchemaSharedCmd(sessionUserId        = sessionUserId.id,
                                         id                   = containerSchema.id.id,
                                         expectedVersion      = containerSchema.version,
                                         shared               = !containerSchema.shared),
          UpdateContainerSchemaCentreCmd(sessionUserId        = sessionUserId.id,
                                         id                   = containerSchema.id.id,
                                         expectedVersion      = containerSchema.version,
                                         centreId             = containerSchema.centreId.id),
          UpdateContainerSchemaLabelsCmd(sessionUserId        = sessionUserId.id,
                                         id                   = containerSchema.id.id,
                                         expectedVersion      = containerSchema.version,
                                         labels               = List(nameGenerator.next[ContainerSchema])))

  override protected def addToRepository[E <: ConcurrencySafeEntity[_]](entity: E): Unit =
    entity match {
      case u: User            => userRepository.put(u)
      case i: AccessItem      => accessItemRepository.put(i)
      case m: Membership      => membershipRepository.put(m)
      case c: Centre          => centreRepository.put(c)
      case c: ContainerSchema => schemaRepository.put(c)
      case e => fail(s"invalid entity: $e")
    }

  protected def addAllToRepository(entities: Set[ConcurrencySafeEntity[_]]): Unit =
    entities.foreach(addToRepository)

  protected def persistRoles(f: ContainerSchemaAccessFixture): Unit = {
    addUserToCentreAdminRole(f.allCentresAdminUser)
    addUserToCentreAdminRole(f.centreOnlyAdminUser)
    addUserToRole(f.centreUser, RoleId.CentreUser)
    addUserToRole(f.noMembershipUser, RoleId.CentreUser)
  }

  protected def addUserToCentreAdminRole(user: User): Unit =
    addUserToRole(user, RoleId.CentreAdministrator)

}

class ContainerSchemaAccessFixture(factory: Factory) extends UsersWithCentreAccessFixture(factory) {

  val schema = factory.createContainerSchema.copy(centreId = centre.id)

  override def allEntities(): Set[ConcurrencySafeEntity[_]] = super.allEntities + schema

  def allEntitiesButSchema: Set[ConcurrencySafeEntity[_]] = super.allEntities

  def createSibling(factory: Factory) = factory.createContainerSchema.copy(centreId = centre.id)

}
