package org.biobank.services.centres

import akka.actor.ActorRef
import com.google.inject.ImplementedBy
import javax.inject.{Inject, Named}
//import org.biobank.domain.centres.{CentreId}
import org.biobank.domain.containers.ContainerTypeRepository
//import org.biobank.domain.users.UserId
//import org.biobank.dto._
import org.biobank.services.BbwebService
import org.biobank.services._
import org.biobank.services.access.AccessService
import org.slf4j.{Logger, LoggerFactory}
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent._
//import scalaz.Scalaz._
//import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerTypesServiceImpl])
trait ContainerTypesService extends BbwebService {}

class ContainerTypesServiceImpl @Inject()(
    @Named("containerTypesProcessor") val processor: ActorRef,
    val accessService:                               AccessService,
    val containerTypeRepository:                     ContainerTypeRepository)
    extends ContainerTypesService {

  //import org.biobank.CommonValidations._
  //import org.biobank.domain.access.AccessItem._

  val log: Logger = LoggerFactory.getLogger(this.getClass)
}
