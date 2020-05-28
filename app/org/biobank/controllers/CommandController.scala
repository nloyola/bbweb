package org.biobank.controllers

import cats.data.Validated.{Invalid, Valid}
import org.biobank._
import org.biobank.infrastructure.commands.Commands._
import org.biobank.services.ServiceValidation
import org.biobank.validation.Validation._
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}

object CommandController {

  final case class UpdateEntityJson(
      sessionUserId:   String,
      id:              String,
      expectedVersion: Long,
      property:        String,
      newValue:        JsValue)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit val updateEntityJsonFormat: Format[UpdateEntityJson] = Json.format[UpdateEntityJson]
}

/**
 *  Uses [[https://github.com/omniti-labs/jsend JSend]] format for JSon replies.
 */
@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
abstract class CommandController(controllerComponents: ControllerComponents)
    extends AbstractController(controllerComponents) {

  implicit protected val ec: ExecutionContext

  protected val action: BbwebAction

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  protected def commandAction[T <: Command](
      jsonExtra: JsValue
    )(block:     T => Future[Result]
    )(
      implicit
      reads: Reads[T]
    ): Action[JsValue] =
    action.async(parse.json) { request =>
      var jsonCmd = request.body.as[JsObject] ++ Json.obj("sessionUserId" -> request.identity.user.id)
      if (jsonExtra != JsNull) {
        jsonCmd = jsonCmd ++ jsonExtra.as[JsObject]
      }
      processJsonCommand(jsonCmd)(block)
    }

  /**
   * This is for actions that do not require the user to be logged in.
   */
  protected def anonymousCommandAction[T <: Command](
      block: T => Future[Result]
    )(
      implicit
      reads: Reads[T]
    ): Action[JsValue] =
    Action.async(parse.json) { request =>
      processJsonCommand(request.body.as[JsObject])(block)
    }

  protected def processJsonCommand[T <: Command](
      jsonCmd: JsValue
    )(block:   T => Future[Result]
    )(
      implicit
      reads: Reads[T]
    ): Future[Result] = {
    if (log.isTraceEnabled) {
      log.trace(s"commandAction: json: $jsonCmd")
    }

    jsonCmd
      .validate[T].fold(errors => {
        val errString = errors.map(e => s"field: ${e._1}, errors: ${e._2}").toList.mkString(", ")
        Future.successful(BadRequest(Json.obj("status" -> "error", "message" -> errString)))
      }, cmd => block(cmd))
  }

  protected def errorReplyJson(message: String): JsValue =
    Json.obj("status" -> "error", "message" -> message)

  protected def BadRequest(message: String): Result = Results.BadRequest(errorReplyJson(message))

  protected def Forbidden(message: String): Result = Results.Forbidden(errorReplyJson(message))

  protected def NotFound(message: String): Result = Results.NotFound(errorReplyJson(message))

  protected def InternalServerError(message: String): Result =
    Results.InternalServerError(errorReplyJson(message))

  protected def Ok[T](data: T)(implicit writes: Writes[T]): Result =
    Results.Ok(Json.obj("status" -> "success", "data" -> Json.toJson[T](data)))

  protected def validationReply[T](validation: ServiceValidation[T])(implicit writes: Writes[T]): Result =
    validation
      .fold(err => errorReply(err.list.toList.mkString(", ")), reply => {
        Ok(reply)
      })

  protected def validationReply[T](
      future: Future[ServiceValidation[T]]
    )(
      implicit
      writes: Writes[T]
    ): Future[Result] =
    future.map { validation =>
      validationReply(validation)
    }

  protected def validationReply[T](
      validation: FutureValidation[T]
    )(
      implicit
      writes: Writes[T]
    ): Future[Result] =
    validation.futval.map { validation =>
      validationReply(validation)
    }

  protected def validationReply[T](
      validation: ValidationResult[T]
    )(
      implicit
      writes: Writes[T]
    ): Result = {
    validation match {
      case Invalid(err) => BadRequest(err.toString)
      case Valid(r)     => Ok(r)
    }
  }

  protected def validationReply[T](
      validation: FutureValidationResult[T]
    )(
      implicit
      writes: Writes[T]
    ): Future[Result] = {
    validation.value.map { v =>
      v match {
        case Left(err) => errorReply(err.toString)
        case Right(r)  => Ok(r)
      }
    }
  }

  private def errorReply(errors: String): Result = {
    if (errors.contains("Unauthorized")) {
      Unauthorized
    } else if (("NotFound".r.findAllIn(errors).length > 0)) {
      NotFound(errors)
    } else if (errors.contains("already exists")) {
      Forbidden(errors)
    } else {
      BadRequest(errors)
    }
  }

}
