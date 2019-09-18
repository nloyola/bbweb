package org.biobank.controllers.centres

import javax.inject.{Inject, Singleton}
import org.biobank.controllers._
import org.biobank.dto.ContainerDto
import org.biobank.domain.Slug
import org.biobank.domain.centres.CentreId
import org.biobank.domain.containers.ContainerId
import org.biobank.services.PagedResults
import org.biobank.services.centres.ContainersService
import play.api.libs.json._
import play.api.mvc._
import play.api.{Environment, Logger}
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._

/**
 *  Uses [[https://github.com/omniti-labs/jsend JSend]] format for JSon replies.
 */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
@Singleton
class ContainersController @Inject()(
    controllerComponents: ControllerComponents,
    val action:           BbwebAction,
    val env:              Environment,
    val service:          ContainersService
  )(
    implicit
    val ec: ExecutionContext)
    extends CommandController(controllerComponents) {

  import org.biobank.infrastructure.commands.ContainerCommands._

  val log: Logger = Logger(this.getClass)

  private val PageSizeMax = 10

  def getBySlug(slug: Slug): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val v = service.getBySlug(request.identity.user.id, slug)
      validationReply(v)
    }

  def search(centreId: CentreId): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      PagedQueryHelper(request.rawQueryString, PageSizeMax).fold(err => {
        validationReply(Future.successful(err.failure[PagedResults[ContainerDto]]))
      }, pagedQuery => {
        validationReply(service.search(request.identity.user.id, centreId, pagedQuery))
      })
    }

  def getChildrenBySlug(slug: Slug): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val v = service.getChildrenBySlug(request.identity.user.id, slug)
      validationReply(v)
    }

  def addRootContainer(): Action[JsValue] =
    commandAction[AddRootContainerCmd](JsNull)(processCommand)

  def addStorageContainer(): Action[JsValue] =
    commandAction[AddStorageContainerCmd](JsNull)(processCommand)

  def addSpecimenContainer(): Action[JsValue] =
    commandAction[AddSpecimenContainerCmd](JsNull)(processCommand)

  def updateLabel(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerLabelCmd](Json.obj("id" -> id))(processCommand)

  def updateInventoryId(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerInventoryIdCmd](Json.obj("id" -> id))(processCommand)

  def updateEnabled(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerEnabledCmd](Json.obj("id" -> id))(processCommand)

  def updateContainerType(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerContainerTypeCmd](Json.obj("id" -> id))(processCommand)

  def updateCentreLocation(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerCentreLocationCmd](Json.obj("id" -> id))(processCommand)

  def updateTemperature(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerTemperatureCmd](Json.obj("id" -> id))(processCommand)

  def updateConstraints(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerConstraintsCmd](Json.obj("id" -> id))(processCommand)

  def removeConstraints(id: ContainerId): Action[JsValue] =
    commandAction[RemoveContainerConstraintsCmd](Json.obj("id" -> id))(processCommand)

  def updatePosition(id: ContainerId): Action[JsValue] =
    commandAction[UpdateContainerPositionCmd](Json.obj("id" -> id))(processCommand)

  def remove(schemaId: ContainerId, version: Long): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val cmd = RemoveContainerCmd(sessionUserId = request.identity.user.id.id,
                                   id              = schemaId.id,
                                   expectedVersion = version)
      val future = service.processRemoveCommand(cmd)
      validationReply(future)
    }

  def snapshot: Action[Unit] =
    action(parse.empty) { implicit request =>
      validationReply(service.snapshotRequest(request.identity.user.id).map(_ => true))
    }

  private def processCommand(cmd: ContainerCommand): Future[Result] = {
    val future = service.processCommand(cmd)
    validationReply(future)
  }

}
