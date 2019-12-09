package org.biobank.services.centres

import org.biobank._
import org.biobank.domain.access._
import org.biobank.domain.users.UserId
import org.biobank.domain.centres._
import org.biobank.services._
import org.biobank.services.ServicePermissionChecks
import org.slf4j.Logger
import scala.concurrent.ExecutionContext
import scalaz._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

trait CentreServicePermissionChecks extends ServicePermissionChecks {

  import org.biobank.domain.access.AccessItem._
  import org.biobank.domain.access.PermissionId._

  val log: Logger

  implicit val executionContext: ExecutionContext

  protected val centreRepository: CentreRepository

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  private def withPermittedCentres[T](
      requestUserId: UserId,
      permissionId:  PermissionId
    )(block:         Set[Centre] => ServiceValidation[T]
    ): ServiceValidation[T] =
    whenPermitted(requestUserId, permissionId) { () =>
      for {
        centres <- getMembershipCentres(requestUserId)
        result  <- block(centres)
      } yield result
    }

  protected def withPermittedCentres[T](
      requestUserId: UserId
    )(block:         Set[Centre] => ServiceValidation[T]
    ): ServiceValidation[T] =
    withPermittedCentres(requestUserId, PermissionId.CentreRead)(block)

  protected def withPermittedShippingCentres[T](
      requestUserId: UserId
    )(block:         Set[Centre] => ServiceValidation[T]
    ): ServiceValidation[T] =
    withPermittedCentres(requestUserId, PermissionId.ShipmentRead)(block)

  protected def withPermittedShippingCentresAsync[T](
      userId: UserId
    )(block:  Set[Centre] => FutureValidation[T]
    ): FutureValidation[T] = {
    whenPermittedAsync(userId, PermissionId.ShipmentRead) { () =>
      getMembershipCentres(userId) match {
        case Success(centres) => block(centres)
        case Failure(err)     => FutureValidation(err.failure[T])
      }
    }
  }

  protected def permittedShippingCentresAsync(userId: UserId): FutureValidation[Set[Centre]] = {
    FutureValidation {
      for {
        permitted <- accessService.hasPermission2(userId, PermissionId.ShipmentRead)
        centres   <- getMembershipCentres(userId)
      } yield centres
    }
  }

  protected def getMembershipCentres(userId: UserId): ServiceValidation[Set[Centre]] =
    accessService.getUserMembership(userId).flatMap { membership =>
      if (membership.centreData.allEntities) {
        centreRepository.getValues.toSet.successNel[String]
      } else {
        membership.centreData.ids
          .map(centreRepository.getByKey)
          .toList.sequenceU
          .map(centres => centres.toSet)
      }
    }

}
