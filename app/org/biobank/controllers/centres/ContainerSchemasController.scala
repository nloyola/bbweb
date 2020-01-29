package org.biobank.controllers.centres

import javax.inject.{Inject, Singleton}
import org.biobank.controllers._
import org.biobank.dto.containers.ContainerSchemaDto
import org.biobank.domain.Slug
import org.biobank.domain.centres.CentreId
import org.biobank.domain.containers.ContainerSchemaId
import org.biobank.services.centres.ContainerSchemasService
import play.api.libs.json._
import play.api.mvc._
import play.api.{Environment, Logger}
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
import org.biobank.services.PagedResults

/**
 *  Uses [[https://github.com/omniti-labs/jsend JSend]] format for JSon replies.
 */
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
@Singleton
class ContainerSchemasController @Inject()(
    controllerComponents: ControllerComponents,
    val action:           BbwebAction,
    val env:              Environment,
    val service:          ContainerSchemasService
  )(
    implicit
    val ec: ExecutionContext)
    extends CommandController(controllerComponents) {

  import org.biobank.infrastructure.commands.ContainerSchemaCommands._

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
        validationReply(Future.successful(err.failure[PagedResults[ContainerSchemaDto]]))
      }, pagedQuery => {
        validationReply(service.search(request.identity.user.id, centreId, pagedQuery))
      })
    }

  def addSchema(): Action[JsValue] =
    commandAction[AddContainerSchemaCmd](JsNull)(processCommand)

  def updateName(id: ContainerSchemaId): Action[JsValue] =
    commandAction[UpdateContainerSchemaNameCmd](Json.obj("id" -> id))(processCommand)

  def updateDescription(id: ContainerSchemaId): Action[JsValue] =
    commandAction[UpdateContainerSchemaDescriptionCmd](Json.obj("id" -> id))(processCommand)

  def updateShared(id: ContainerSchemaId): Action[JsValue] =
    commandAction[UpdateContainerSchemaSharedCmd](Json.obj("id" -> id))(processCommand)

  def updateCentre(id: ContainerSchemaId): Action[JsValue] =
    commandAction[UpdateContainerSchemaCentreCmd](Json.obj("id" -> id))(processCommand)

  def updateLabels(id: ContainerSchemaId): Action[JsValue] =
    commandAction[UpdateContainerSchemaLabelsCmd](Json.obj("id" -> id))(processCommand)

  def remove(schemaId: ContainerSchemaId, version: Long): Action[Unit] =
    action.async(parse.empty) { implicit request =>
      val cmd = RemoveContainerSchemaCmd(sessionUserId = request.identity.user.id.id,
                                         id              = schemaId.id,
                                         expectedVersion = version)
      val future = service.processRemoveCommand(cmd)
      validationReply(future)
    }

  private def processCommand(cmd: ContainerSchemaCommand): Future[Result] = {
    val future = service.processCommand(cmd)
    validationReply(future)
  }

  def snapshot: Action[Unit] =
    action(parse.empty) { implicit request =>
      validationReply(service.snapshotRequest(request.identity.user.id).map(_ => true))
    }

}
