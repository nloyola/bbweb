package org.biobank.services

import akka.actor.ActorLogging
import akka.persistence.PersistentActor
import scalapb.GeneratedMessage
import org.biobank.domain._
import scalaz._
import scalaz.Scalaz._

trait Processor extends PersistentActor with ActorLogging {
  import org.biobank.CommonValidations._

  /**
   * Persists the event passed in the validation if it is successful. In either case
   * the sender is sent either the success or failure validation.
   *
   * @see http://helenaedelson.com/?p=879
   *
   */
  // FIXME: remove this when entire query side is implemented
  protected def process[T <: GeneratedMessage](
      validation: ServiceValidation[T]
    )(successFn:  T => Unit
    ): Unit = {
    val originalSender = context.sender
    validation.fold(
      err => {
        originalSender ! validation // inform the sender of the failure
      },
      event => {
        persist(event) { ev =>
          successFn(ev)
          originalSender ! validation // inform the sender of the successful event resulting from a valid command
        }
      }
    )
  }

  // FIXME: rename to "process" when entire query side is implemented
  protected def processWithResult[T <: GeneratedMessage, E <: IdentifiedDomainObject[_]](
      validation: ServiceValidation[T]
    )(successFn:  T => ServiceValidation[E]
    ): Unit = {
    val originalSender = context.sender
    validation
      .fold(err => originalSender ! validation, // inform the sender of the failure
            event =>
              persist(event) { ev =>
                val result = successFn(ev)
                result match {
                  case Failure(err) => log.error(err.toString)
                  case _            =>
                }

                // inform the sender of the successful event resulting from a valid command
                originalSender ! result
              })
  }

  protected def validNewIdentity[I <: IdentifiedValueObject[_], R <: ReadWriteRepository[I, _]](
      id:         I,
      repository: R
    ): ServiceValidation[I] =
    repository
      .getByKey(id)
      .fold(err  => id.successNel[String],
            item => ServiceError(s"could not generate a unique ID: $id").failureNel[I])

  /**
   * Searches the repository for a matching item.
   */
  protected def nameAvailableMatcher[T <: IdentifiedDomainObject[_]](
      name:         String,
      repository:   ReadRepository[_, T],
      errMsgPrefix: String
    )(predicate:    T => Boolean
    ): ServiceValidation[Unit] =
    if (repository.exists(predicate)) EntityCriteriaError(s"$errMsgPrefix: $name").failureNel[Unit]
    else ().successNel[String]

  /** Checks that the domain objects version matches the expected one.
   */
  // protected def validateVersion[T <: ConcurrencySafeEntity[_]](
  //     item:            T,
  //     expectedVersion: Option[Long]
  //   ): ServiceValidation[Unit] =
  //   if (item.versionOption == expectedVersion) ().successNel[String]
  //   else ServiceError(s"version mismatch").failureNel[Unit]

}
