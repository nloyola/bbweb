package org.biobank.controllers.participants

import javax.inject.{Inject, Singleton}
import org.biobank.controllers._
import org.biobank.domain.participants.{CollectionEventId, ParticipantId}
import org.biobank.dto.CollectionEventDto
import org.biobank.infrastructure.commands.CollectionEventCommands._
import org.biobank.services.PagedResults
import org.biobank.services.participants.CollectionEventsService
import play.api.libs.json._
import play.api.{Environment, Logger}
import play.api.mvc.{Action, ControllerComponents, Result}
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._

@Singleton
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class CollectionEventsController @Inject()(
    controllerComponents: ControllerComponents,
    val action:           BbwebAction,
    val env:              Environment,
    val service:          CollectionEventsService
  )(
    implicit
    val ec: ExecutionContext)
    extends CommandController(controllerComponents) {

  val log: Logger = Logger(this.getClass)

  private val PageSizeMax = 10

  def get(ceventId: String): Action[Unit] =
    action(parse.empty) { implicit request =>
      validationReply(service.get(request.identity.user.id, CollectionEventId(ceventId)))
    }

  def list(participantId: ParticipantId): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      PagedQueryHelper(request.rawQueryString, PageSizeMax).fold(err => {
        validationReply(Future.successful(err.failure[PagedResults[CollectionEventDto]]))
      }, pagedQuery => {
        validationReply(service.list(request.identity.user.id, participantId, pagedQuery))
      })
    }

  def getByVisitNumber(participantId: ParticipantId, visitNumber: Int): Action[Unit] =
    action(parse.empty) { implicit request =>
      validationReply(service.getByVisitNumber(request.identity.user.id, participantId, visitNumber))
    }

  def snapshot: Action[Unit] =
    action(parse.empty) { implicit request =>
      validationReply(service.snapshotRequest(request.identity.user.id).map(_ => true))
    }

  def add(participantId: ParticipantId): Action[JsValue] =
    commandAction[AddCollectionEventCmd](Json.obj("participantId" -> participantId))(processCommand)

  def updateVisitNumber(ceventId: CollectionEventId): Action[JsValue] =
    commandAction[UpdateCollectionEventVisitNumberCmd](Json.obj("id" -> ceventId))(processCommand)

  def updateTimeCompleted(ceventId: CollectionEventId): Action[JsValue] =
    commandAction[UpdateCollectionEventTimeCompletedCmd](Json.obj("id" -> ceventId))(processCommand)

  def addAnnotation(ceventId: CollectionEventId): Action[JsValue] =
    commandAction[CollectionEventUpdateAnnotationCmd](Json.obj("id" -> ceventId))(processCommand)

  def removeAnnotation(ceventId: CollectionEventId, annotTypeId: String, ver: Long): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val cmd = RemoveCollectionEventAnnotationCmd(sessionUserId = request.identity.user.id.id,
                                                   id               = ceventId.id,
                                                   expectedVersion  = ver,
                                                   annotationTypeId = annotTypeId)
      processCommand(cmd)
    }

  def remove(participantId: ParticipantId, ceventId: CollectionEventId, ver: Long): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val cmd = RemoveCollectionEventCmd(sessionUserId = request.identity.user.id.id,
                                         id              = ceventId.id,
                                         participantId   = participantId.id,
                                         expectedVersion = ver)
      val future = service.processRemoveCommand(cmd)
      validationReply(future)
    }

  private def processCommand(cmd: CollectionEventCommand): Future[Result] = {
    val future = service.processCommand(cmd)
    validationReply(future)
  }

}
