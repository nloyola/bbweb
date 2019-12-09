package org.biobank.services.centres

import akka.actor.ActorRef
import akka.pattern.ask
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank._
import org.biobank.domain.Slug
import org.biobank.domain.access.PermissionId
import org.biobank.domain.centres.{Centre, CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.domain.users.UserId
import org.biobank.dto._
import org.biobank.infrastructure.AscendingOrder
import org.biobank.infrastructure.commands.ContainerSchemaCommands._
import org.biobank.infrastructure.events.ContainerSchemaEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerSchemasServiceImpl])
trait ContainerSchemasService extends BbwebService {

  def getBySlug(requestUserId: UserId, slug: Slug): FutureValidation[ContainerSchemaDto]

  def search(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[ContainerSchemaDto]]

  def processCommand(cmd: ContainerSchemaCommand): FutureValidation[ContainerSchemaDto]

  def processRemoveCommand(cmd: RemoveContainerSchemaCmd): FutureValidation[Boolean]

  def snapshotRequest(requestUserId: UserId): ServiceValidation[Unit]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class ContainerSchemasServiceImpl @Inject()(
    @Named("containerSchemasProcessor") val processor: ActorRef,
    val accessService:                                 AccessService,
    val centreRepository:                              CentreRepository,
    val schemaRepository:                              ContainerSchemaRepository
  )(
    implicit
    val executionContext: ExecutionContext)
    extends ContainerSchemasService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def getBySlug(requestUserId: UserId, slug: Slug): FutureValidation[ContainerSchemaDto] =
    FutureValidation {
      whenSchemaPermitted(requestUserId, slug) { (centre, schema) =>
        ContainerSchemaDto(schema, centre).successNel[String]
      }
    }

  /** All [[domain.containers.ContainerSchema ContainerSchemas]] for a [domain.centres.Centre Centre], and all
   * shared [[domain.containers.ContainerSchema ContainerSchemas]] for other [domain.centres.Centre Centres].
   */
  def search(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): FutureValidation[PagedResults[ContainerSchemaDto]] =
    FutureValidation {
      whenCentrePermitted(requestUserId, centreId) { centre =>
        val allSchemas = schemaRepository.allForCentre(centre.id)
        for {
          schemas   <- filterSchemasInternal(allSchemas, query.filter, query.sort)
          validPage <- query.validPage(schemas.size)
          dtos = schemas.map(s => ContainerSchemaDto(s, centre))
          results <- PagedResults.create(dtos, query.page, query.limit)
        } yield results
      }
    }

  def processCommand(cmd: ContainerSchemaCommand): FutureValidation[ContainerSchemaDto] = {
    val validCentre = cmd match {
      case c: AddContainerSchemaCmd => centreRepository.getByKey(CentreId(c.centreId))
      case c: ContainerSchemaModifyCommand =>
        for {
          schema <- schemaRepository.getByKey(ContainerSchemaId(c.id))
          centre <- centreRepository.getByKey(schema.centreId)
        } yield centre

    }

    val permission = cmd match {
      case c: AddContainerSchemaCmd => PermissionId.ContainerCreate
      case _ => PermissionId.ContainerUpdate
    }

    val requestUserId = UserId(cmd.sessionUserId)

    validCentre
      .fold(err => FutureValidation((err.failure[ContainerSchemaDto])),
            centre =>
              whenPermittedAndIsMemberAsync(requestUserId, permission, None, Some(centre.id)) { () =>
                for {
                  event <- FutureValidation(
                            ask(processor, cmd).mapTo[ServiceValidation[ContainerSchemaEvent]]
                          )
                  // need to retrieve the centre attached to the returned schema, since there is a
                  // possibility it was updated to a new centre
                  schema       <- FutureValidation(schemaRepository.getByKey(ContainerSchemaId(event.id)))
                  schemaCentre <- FutureValidation(centreRepository.getByKey(schema.centreId))
                } yield ContainerSchemaDto(schema, schemaCentre)
              })
  }

  def processRemoveCommand(cmd: RemoveContainerSchemaCmd): FutureValidation[Boolean] = {
    val validCentre = for {
      schema <- schemaRepository.getByKey(ContainerSchemaId(cmd.id))
      centre <- centreRepository.getByKey(schema.centreId)
    } yield centre

    validCentre
      .fold(err => FutureValidation((err.failure[Boolean])),
            centre =>
              whenPermittedAndIsMemberAsync(UserId(cmd.sessionUserId),
                                            PermissionId.ContainerDelete,
                                            None,
                                            Some(centre.id)) { () =>
                FutureValidation(
                  ask(processor, cmd)
                    .mapTo[ServiceValidation[ContainerSchemaEvent]]
                ).map(_ => true)
              })
  }

  private def whenCentrePermitted[T](
      requestUserId: UserId,
      centreId:      CentreId
    )(block:         Centre => ServiceValidation[T]
    ): ServiceValidation[T] =
    for {
      centre <- centreRepository.getByKey(centreId)
      result <- whenPermittedAndIsMember(requestUserId, PermissionId.ContainerRead, None, Some(centre.id))(
                 () => block(centre)
               )
    } yield result

  private def whenSchemaPermitted[T](
      requestUserId: UserId,
      schemaSlug:    Slug
    )(block:         (Centre, ContainerSchema) => ServiceValidation[T]
    ): ServiceValidation[T] = {
    for {
      schema <- schemaRepository.getBySlug(schemaSlug)
      centre <- centreRepository.getByKey(schema.centreId)
      result <- whenPermittedAndIsMember(requestUserId, PermissionId.ContainerRead, None, Some(centre.id))(
                 () => block(centre, schema)
               )
    } yield result
  }

  private def filterSchemasInternal(
      unfilteredSchemas: Set[ContainerSchema],
      filter:            FilterString,
      sort:              SortString
    ): ServiceValidation[Seq[ContainerSchema]] = {
    val sortStr =
      if (sort.expression.isEmpty) new SortString("name")
      else sort

    for {
      schemas <- ContainerSchemaFilter.filterSchemas(unfilteredSchemas, filter)
      sortExpressions <- {
        QuerySortParser(sortStr).toSuccessNel(ServiceError(s"could not parse sort expression: $sort"))
      }
      sortFunc <- {
        ContainerSchema.sort2Compare
          .get(sortExpressions(0).name).toSuccessNel(
            ServiceError(s"invalid sort field: ${sortExpressions(0).name}")
          )
      }
    } yield {
      val result = schemas.toSeq.sortWith(sortFunc)
      if (sortExpressions(0).order == AscendingOrder) result
      else result.reverse
    }
  }
}
