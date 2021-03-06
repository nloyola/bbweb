package org.biobank.services.participants

import org.biobank.fixtures._
import org.biobank.domain.access._
import org.biobank.domain.annotations._
import org.biobank.domain.studies._
import org.biobank.domain.participants._
import org.biobank.domain.users._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.TableDrivenPropertyChecks._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Primarily these are tests that exercise the User Access aspect of StudiesService.
 */
class ParticipantsServiceSpec
    extends ProcessorTestFixture with ParticipantsServiceFixtures with ScalaFutures {

  import org.biobank.TestUtils._
  import org.biobank.infrastructure.commands.ParticipantCommands._

  protected val nameGenerator = new NameGenerator(this.getClass)

  protected val accessItemRepository = app.injector.instanceOf[AccessItemRepository]

  protected val membershipRepository = app.injector.instanceOf[MembershipRepository]

  protected val userRepository = app.injector.instanceOf[UserRepository]

  protected val studyRepository = app.injector.instanceOf[StudyRepository]

  protected val collectionEventTypeRepository = app.injector.instanceOf[CollectionEventTypeRepository]

  protected val participantRepository = app.injector.instanceOf[ParticipantRepository]

  protected val collectionEventRepository = app.injector.instanceOf[CollectionEventRepository]

  private val participantsService = app.injector.instanceOf[ParticipantsService]

  private def commandsTable(sessionUserId: UserId, participant: Participant, annotation: Annotation) =
    Table("participant commands",
          UpdateParticipantUniqueIdCmd(sessionUserId      = sessionUserId.id,
                                       id                 = participant.id.id,
                                       expectedVersion    = participant.version,
                                       uniqueId           = participant.uniqueId),
          ParticipantUpdateAnnotationCmd(sessionUserId    = sessionUserId.id,
                                         id               = participant.id.id,
                                         expectedVersion  = participant.version,
                                         annotationTypeId = annotation.annotationTypeId.id,
                                         stringValue      = annotation.stringValue,
                                         numberValue      = annotation.numberValue,
                                         selectedValues   = annotation.selectedValues),
          ParticipantRemoveAnnotationCmd(sessionUserId    = sessionUserId.id,
                                         id               = participant.id.id,
                                         expectedVersion  = participant.version,
                                         annotationTypeId = annotation.annotationTypeId.id))

  override def beforeEach() = {
    super.beforeEach()
    collectionEventTypeRepository.removeAll
    participantRepository.removeAll
  }

  describe("ParticipantsService") {

    describe("when getting a participant") {

      it("users can access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          participantsService
            .get(user.id, f.enabledStudy.id, f.participant.id)
            .mustSucceed { result =>
              result.id must be(f.participant.id.id)
            }
        }
      }

      it("users cannot access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCannotReadTable) { (user, label) =>
          info(label)
          participantsService.get(user.id, f.enabledStudy.id, f.participant.id) mustFail "Unauthorized"
        }
      }

    }

    describe("when getting a participant by unique ID") {

      it("users can access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCanReadTable) { (user, label) =>
          info(label)
          participantsService
            .getByUniqueId(user.id, f.participant.uniqueId)
            .mustSucceed { result =>
              result.id must be(f.participant.id.id)
            }
        }
      }

      it("users cannot access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCannotReadTable) { (user, label) =>
          info(label)
          participantsService
            .getByUniqueId(user.id, f.participant.uniqueId)
            .mustFail("Unauthorized")
        }
      }

    }

    describe("when adding a participant") {

      it("users can access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          participantRepository.removeAll
          val cmd = AddParticipantCmd(sessionUserId = user.id.id,
                                      studyId     = f.participant.studyId.id,
                                      uniqueId    = f.participant.uniqueId,
                                      annotations = List.empty[Annotation])
          participantsService.processCommand(cmd).mustSucceed { result =>
            result.uniqueId must be(f.participant.uniqueId)
          }
        }
      }

      it("users cannot access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          info(label)
          participantRepository.removeAll
          val cmd = AddParticipantCmd(sessionUserId = user.id.id,
                                      studyId     = f.participant.studyId.id,
                                      uniqueId    = f.participant.uniqueId,
                                      annotations = List.empty[Annotation])
          participantsService.processCommand(cmd) mustFail "Unauthorized"
        }
      }

    }

    describe("when updating participants") {

      it("users can access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCanAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(commandsTable(user.id, f.participant, f.annotation)) { cmd =>
            val participant = cmd match {
              case c: ParticipantUpdateAnnotationCmd =>
                f.participant.copy(annotations = Set.empty[Annotation])
              case _ => f.participant
            }
            participantRepository.put(participant)
            participantsService.processCommand(cmd).mustSucceed { result =>
              result.id must be(f.participant.id.id)
            }
          }
        }
      }

      it("users cannot access") {
        val f = new UsersWithParticipantAccessFixture
        forAll(f.usersCannotAddOrUpdateTable) { (user, label) =>
          info(label)
          forAll(commandsTable(user.id, f.participant, f.annotation)) { cmd =>
            participantsService.processCommand(cmd) mustFail "Unauthorized"
          }
        }
      }

    }

  }
}
