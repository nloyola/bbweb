package org.biobank.services.centres

import akka.actor.ActorRef
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
import org.biobank.domain.centres.{CentreId, CentreRepository}
import org.biobank.domain.containers._
import org.biobank.domain.users.UserId
import org.biobank.dto._
//import org.biobank.infrastructure.commands.ContainerCommands._
//import org.biobank.infrastructure.events.ContainerEvents._
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent._
//import scala.concurrent.ExecutionContext.Implicits.global
//import scalaz.Scalaz._
//import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerSchemasServiceImpl])
trait ContainerSchemasService extends BbwebService

class ContainerSchemasServiceImpl @Inject()(
    @Named("containersProcessor") val processor: ActorRef,
    val accessService:                           AccessService,
    val centresService:                          CentresService,
    val centreRepository:                        CentreRepository,
    val containerSchemaRepository:               ContainerSchemaRepository)
    extends ContainerSchemasService with AccessChecksSerivce with ServicePermissionChecks {

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  /** All Top [[domain.containers.Container Containers]] for a [domain.centres.Centre Centre]. */
  def getSchemas(
      requestUserId: UserId,
      centreId:      CentreId,
      query:         PagedQuery
    ): Future[ServiceValidation[PagedResults[ContainerSchemaDto]]] = ???

}
