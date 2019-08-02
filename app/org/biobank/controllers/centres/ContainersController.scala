package org.biobank.controllers.centres

import javax.inject.{Inject, Singleton}
import org.biobank.controllers._
import org.biobank.domain.Slug
import org.biobank.domain.containers.ContainerId
//import org.biobank.dto._
//import org.biobank.services._
import org.biobank.services.centres.ContainersService
import play.api.libs.json._
import play.api.mvc._
import play.api.{ Environment, Logger }
import scala.concurrent.{ExecutionContext, Future}
//import scalaz.Scalaz._

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
) (
  implicit val ec: ExecutionContext
)
    extends CommandController(controllerComponents) {

  import org.biobank.infrastructure.commands.ContainerCommands._

  val log: Logger = Logger(this.getClass)

  def list: Action[Unit] = ???

  def getBySlug(slug: Slug): Action[Unit] = ???

  def addRootContainer(): Action[JsValue] =
    commandAction[AddRootContainerCmd](JsNull)(processCommand)

  def addStorageContainer(): Action[JsValue] =
    commandAction[AddStorageContainerCmd](JsNull)(processCommand)

  def addSpecimenContainer(): Action[JsValue] =
    commandAction[AddSpecimenContainerCmd](JsNull)(processCommand)

  def update(id: ContainerId): Action[JsValue] = ???

  def snapshot: Action[Unit] = ???

  private def processCommand(cmd: ContainerCommand): Future[Result] = {
    val future = service.processCommand(cmd)
    validationReply(future)
  }


}
