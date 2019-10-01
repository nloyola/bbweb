package org.biobank.services.centres

import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.studies._
import org.biobank.domain.users._
import org.biobank.fixtures._
import org.biobank.services.users.UserServiceFixtures
import org.scalatest.prop.TableDrivenPropertyChecks._

trait UserCentreAccessFixture {
  val location:               Location
  val centre:                 Centre
  val allCentresAdminUser:    User
  val centreOnlyAdminUser:    User
  val centreUser:             User
  val noMembershipUser:       User
  val noCentrePermissionUser: User
  val allCentresMembership:   Membership
  val centreOnlyMembership:   Membership
  val noCentresMembership:    Membership

  def usersCanReadTable() =
    Table(("users with read access", "label"),
          (allCentresAdminUser, "all centres admin user"),
          (centreOnlyAdminUser, "centre only admin user"),
          (centreUser, "non-admin centre user"))

  def usersCanAddOrUpdateTable() =
    Table(("users with update access", "label"),
          (allCentresAdminUser, "all centres admin user"),
          (centreOnlyAdminUser, "centre only admin user"))

  def usersCannotAddOrUpdateTable() =
    Table(("users with update access", "label"),
          (centreUser, "non-admin centre user"),
          (noMembershipUser, "all centres admin user"),
          (noCentrePermissionUser, "centre only admin user"))

  def allEntities(): Set[ConcurrencySafeEntity[_]] =
    Set(centre,
        allCentresAdminUser,
        centreOnlyAdminUser,
        centreUser,
        noMembershipUser,
        noCentrePermissionUser,
        allCentresMembership,
        centreOnlyMembership,
        noCentresMembership)

}

class UsersWithCentreAccessFixture(factory: Factory) extends UserCentreAccessFixture {
  val location               = factory.createLocation
  val centre                 = factory.createDisabledCentre.copy(locations = Set(location))
  val allCentresAdminUser    = factory.createActiveUser
  val centreOnlyAdminUser    = factory.createActiveUser
  val centreUser             = factory.createActiveUser
  val noMembershipUser       = factory.createActiveUser
  val noCentrePermissionUser = factory.createActiveUser

  val allCentresMembership = factory.createMembership.copy(
    userIds    = Set(allCentresAdminUser.id),
    studyData  = MembershipEntitySet(true, Set.empty[StudyId]),
    centreData = MembershipEntitySet(true, Set.empty[CentreId])
  )

  val centreOnlyMembership = factory.createMembership.copy(
    userIds    = Set(centreOnlyAdminUser.id, centreUser.id),
    studyData  = MembershipEntitySet(true, Set.empty[StudyId]),
    centreData = MembershipEntitySet(false, Set(centre.id))
  )

  val noCentresMembership = factory.createMembership.copy(
    userIds    = Set(noMembershipUser.id, noCentrePermissionUser.id),
    centreData = MembershipEntitySet(false, Set.empty[CentreId])
  )
}

trait CentresServiceFixtures extends ProcessorTestFixture with UserServiceFixtures {

  protected val factory: Factory

  protected val accessItemRepository: AccessItemRepository

  protected val membershipRepository: MembershipRepository

  protected val userRepository: UserRepository

  protected val centreRepository: CentreRepository

  protected val studyRepository: StudyRepository

  protected def addUserToCentreAdminRole(user: User): Unit =
    addUserToRole(user, RoleId.CentreAdministrator)

  protected def createFixture(): UsersWithCentreAccessFixture = {
    val f = new UsersWithCentreAccessFixture(factory)
    f.allEntities.foreach(addToRepository)
    persistRoles(f)
    f
  }

  protected def persistRoles(f: UsersWithCentreAccessFixture): Unit = {
    addUserToCentreAdminRole(f.allCentresAdminUser)
    addUserToCentreAdminRole(f.centreOnlyAdminUser)
    addUserToRole(f.centreUser, RoleId.CentreUser)
    addUserToRole(f.noMembershipUser, RoleId.CentreUser)
  }

  override protected def addToRepository[T <: ConcurrencySafeEntity[_]](entity: T): Unit =
    entity match {
      case u: User       => userRepository.put(u)
      case i: AccessItem => accessItemRepository.put(i)
      case m: Membership => membershipRepository.put(m)
      case c: Centre     => centreRepository.put(c)
      case s: Study      => studyRepository.put(s)
      case e => fail(s"invalid entity: $e")
    }

}
