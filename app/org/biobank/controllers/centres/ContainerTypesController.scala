package org.biobank.controllers.centres

import javax.inject.{Inject, Singleton}
import org.biobank.controllers._
import org.biobank.dto.containers.ContainerTypeDto
import org.biobank.domain.Slug
import org.biobank.domain.centres.CentreId
import org.biobank.domain.containers.ContainerTypeId
import org.biobank.services.PagedResults
import org.biobank.services.centres.ContainerTypesService
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
class ContainerTypesController @Inject()(
    controllerComponents: ControllerComponents,
    val action:           BbwebAction,
    val env:              Environment,
    val service:          ContainerTypesService
  )(
    implicit
    val ec: ExecutionContext)
    extends CommandController(controllerComponents) {

  import org.biobank.infrastructure.commands.ContainerTypeCommands._

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
        validationReply(Future.successful(err.failure[PagedResults[ContainerTypeDto]]))
      }, pagedQuery => {
        validationReply(service.search(request.identity.user.id, centreId, pagedQuery))
      })
    }

  def addStorage(): Action[JsValue] =
    commandAction[AddStorageContainerTypeCmd](JsNull)(processCommand)

  def addSpecimen(): Action[JsValue] =
    commandAction[AddSpecimenContainerTypeCmd](JsNull)(processCommand)

  def updateName(id: ContainerTypeId): Action[JsValue] =
    commandAction[UpdateContainerTypeNameCmd](Json.obj("id" -> id))(processCommand)

  def updateDescription(id: ContainerTypeId): Action[JsValue] =
    commandAction[UpdateContainerTypeDescriptionCmd](Json.obj("id" -> id))(processCommand)

  def updateCentre(id: ContainerTypeId): Action[JsValue] =
    commandAction[UpdateContainerTypeCentreCmd](Json.obj("id" -> id))(processCommand)

  def updateSchema(id: ContainerTypeId): Action[JsValue] =
    commandAction[UpdateContainerTypeSchemaCmd](Json.obj("id" -> id))(processCommand)

  def updateShared(id: ContainerTypeId): Action[JsValue] =
    commandAction[UpdateContainerTypeSharedCmd](Json.obj("id" -> id))(processCommand)

  def updateEnabled(id: ContainerTypeId): Action[JsValue] =
    commandAction[UpdateContainerTypeEnabledCmd](Json.obj("id" -> id))(processCommand)

  def remove(schemaId: ContainerTypeId, version: Long): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val cmd = RemoveContainerTypeCmd(sessionUserId = request.identity.user.id.id,
                                       id              = schemaId.id,
                                       expectedVersion = version)
      val future = service.processRemoveCommand(cmd)
      validationReply(future)
    }

  private def processCommand(cmd: ContainerTypeCommand): Future[Result] = {
    val future = service.processCommand(cmd)
    validationReply(future)
  }

  def snapshot: Action[Unit] =
    action(parse.empty) { implicit request =>
      validationReply(service.snapshotRequest(request.identity.user.id).map(_ => true))
    }

}
