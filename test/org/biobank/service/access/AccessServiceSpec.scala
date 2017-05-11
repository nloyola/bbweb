package org.biobank.service.access

import org.biobank.fixture._
import org.biobank.domain.access._
import org.biobank.domain.user.{ActiveUser, UserRepository}
import org.biobank.domain.study.{StudyId, StudyRepository}
import org.biobank.domain.centre.{CentreId, CentreRepository}

class AccessServiceSpec extends TestFixture with AccessServiceFixtures {

  import org.biobank.TestUtils._

  val accessItemRepository = app.injector.instanceOf[AccessItemRepository]

  val membershipRepository = app.injector.instanceOf[MembershipRepository]

  val userRepository = app.injector.instanceOf[UserRepository]

  val studyRepository = app.injector.instanceOf[StudyRepository]

  val centreRepository = app.injector.instanceOf[CentreRepository]

  val accessService = app.injector.instanceOf[AccessService]

  case class PermissionFixtureParam(user: ActiveUser, role: Role, permission: Permission)

  def permissionFixture() = {
    val f = PermissionFixtureParam(user       = factory.createActiveUser,
                                   role       = factory.createRole,
                                   permission = factory.createPermission)
    Set(f.user, f.role, f.permission).foreach(addToRepository)
    f
  }

  override def beforeEach() {
    accessItemRepository.removeAll
    membershipRepository.removeAll
    super.beforeEach()
  }

  describe("Access Service") {

    describe("hasPermission") {

      it("allows access to a user that has permission through a role") {
        val f = permissionFixture
        val role = f.role.copy(userIds = Set(f.user.id))
        val permission = f.permission.copy(parentIds = Set(role.id))

        Set(f.user, role, permission).foreach(addToRepository)

        accessService.hasPermission(f.user.id, permission.id) mustSucceed { _ must be (true) }
      }

      it("allows access to a user through multiple roles") {
        val f = permissionFixture
        val parentRole = f.role.copy(userIds = Set(f.user.id))
        val childRole = factory.createRole.copy(parentIds = Set(parentRole.id))
        val permission = f.permission.copy(parentIds = Set(childRole.id))

        Set(f.user, parentRole, childRole, permission).foreach(addToRepository)

        accessService.hasPermission(f.user.id, permission.id) mustSucceed { _ must be (true) }
      }

      it("allows access to a user through role that has multiple permissions") {
        val f = permissionFixture
        val role = f.role.copy(userIds = Set(f.user.id))
        val permission1 = f.permission.copy(parentIds = Set(f.role.id))
        val permission2 = factory.createPermission.copy(parentIds = Set(f.role.id))

        Set(f.user, role, permission1, permission2).foreach(addToRepository)

        accessService.hasPermission(f.user.id, permission2.id) mustSucceed { _ must be (true) }
      }

      it("allows access to a user through different roles from a permission") {
        val f = permissionFixture
        val user1 = f.user
        val user2 = factory.createActiveUser
        val role1 = f.role.copy(userIds = Set(user1.id))
        val role2 = factory.createRole.copy(userIds = Set(user2.id))
        val permission = f.permission.copy(parentIds = Set(role1.id, role2.id))

        Set(user1, user2, role1, role2, permission).foreach(addToRepository)

        accessService.hasPermission(user1.id, permission.id) mustSucceed { _ must be (true) }

        accessService.hasPermission(user2.id, permission.id) mustSucceed { _ must be (true) }
      }

      it("allows access to a user through common role") {
        val f = permissionFixture
        val user1 = f.user
        val user2 = factory.createActiveUser
        val parentRole1 = f.role.copy(userIds = Set(user1.id))
        val parentRole2 = factory.createRole.copy(userIds = Set(user2.id))
        val childRole = factory.createRole.copy(parentIds = Set(parentRole1.id, parentRole2.id))
        val permission = f.permission.copy(parentIds = Set(childRole.id))

        Set(user1, user2, parentRole1, parentRole2, childRole, permission).foreach(addToRepository)

        accessService.hasPermission(user1.id, permission.id) mustSucceed { _ must be (true) }

        accessService.hasPermission(user2.id, permission.id) mustSucceed { _ must be (true) }
      }

      it("forbids access to a user does not have permission") {
        val f = permissionFixture
        val role = f.role.copy(userIds = Set(f.user.id))
        val permission = f.permission.copy(parentIds = Set(role.id))

        Set(f.user, role, permission).foreach(addToRepository)

        val user2 = factory.createActiveUser
        accessService.hasPermission(user2.id, permission.id) mustFail "Unauthorized"
      }

    }

    describe("for isMember") {

      it("allows a user that is a member of a study and centre") {
        val f = membershipFixture
        accessService.isMember(f.user.id, Some(f.study.id), Some(f.centre.id)) mustSucceed { _ must be (true) }
        accessService.isMember(f.user.id, Some(f.study.id), None) mustSucceed { _ must be (true) }
        accessService.isMember(f.user.id, None, Some(f.centre.id)) mustSucceed { _ must be (true) }
        accessService.isMember(f.user.id, None, None) mustSucceed { _ must be (false) }
      }

      it("allows user that is member of all studies and all centres") {
        val f = membershipFixture
        val membership = f.membership.copy(userIds    = Set(f.user.id),
                                           studyInfo  = MembershipStudyInfo(true, Set.empty[StudyId]),
                                           centreInfo = MembershipCentreInfo(true, Set.empty[CentreId]))

        addToRepository(membership)
        accessService.isMember(f.user.id, Some(f.study.id), Some(f.centre.id)) mustSucceed { _ must be (true) }
        accessService.isMember(f.user.id, Some(f.study.id), None) mustSucceed { _ must be (true) }
        accessService.isMember(f.user.id, None, Some(f.centre.id)) mustSucceed { _ must be (true) }
        accessService.isMember(f.user.id, None, None) mustSucceed { _ must be (false) }
      }

      it("forbids a user that is not a member of a study and centre") {
        val f = membershipFixture
        val membership = f.membership.copy(userIds    = Set(f.user.id),
                                           studyInfo  = MembershipStudyInfo(false, Set.empty[StudyId]),
                                           centreInfo = MembershipCentreInfo(false, Set.empty[CentreId]))
        addToRepository(membership)

        accessService.isMember(f.user.id, Some(f.study.id), Some(f.centre.id)) mustSucceed { _ must be (false) }
        accessService.isMember(f.user.id, Some(f.study.id), None) mustSucceed { _ must be (false) }
        accessService.isMember(f.user.id, None, Some(f.centre.id)) mustSucceed { _ must be (false) }
        accessService.isMember(f.user.id, None, None) mustSucceed { _ must be (false) }
      }

      it("fails if user not found") {
        val f = membershipFixture
        val membership = factory.createMembership.copy(userIds = Set(f.user.id))
        addToRepository(membership)

        val user2 = factory.createActiveUser
        accessService.isMember(user2.id, Some(f.study.id), Some(f.centre.id))
          .mustFail(s"IdNotFound: user id: ${user2.id}")

        accessService.isMember(user2.id, Some(f.study.id), None) mustFail s"IdNotFound: user id: ${user2.id}"
        accessService.isMember(user2.id, None, Some(f.centre.id)) mustFail s"IdNotFound: user id: ${user2.id}"
      }

      it("fails if study not found") {
        val f = membershipFixture
        val membership = factory.createMembership.copy(userIds = Set(f.user.id))
        addToRepository(membership)

        val study2 = factory.createEnabledStudy
        accessService.isMember(f.user.id, Some(study2.id), Some(f.centre.id))
          .mustFail(s"IdNotFound: study id: ${study2.id}")

        accessService.isMember(f.user.id, Some(study2.id), None)
          .mustFail(s"IdNotFound: study id: ${study2.id}")
      }

      it("fails if centre not found") {
        val f = membershipFixture
        val membership = factory.createMembership.copy(userIds = Set(f.user.id))
        addToRepository(membership)

        val centre2 = factory.createEnabledCentre
        accessService.isMember(f.user.id, Some(f.study.id), Some(centre2.id))
          .mustFail(s"IdNotFound: centre id: ${centre2.id}")

        accessService.isMember(f.user.id, None, Some(centre2.id))
          .mustFail(s"IdNotFound: centre id: ${centre2.id}")
      }

    }

  }

}
