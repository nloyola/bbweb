package org.biobank.domain.containers

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
//import org.biobank.domain.centres.CentreId
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerSchemaRepositoryImpl])
trait ContainerSchemaRepository extends ReadWriteRepository[ContainerSchemaId, ContainerSchema] {

  def getPosition(schemaId: ContainerSchemaId, label: String): DomainValidation[ContainerSchemaPosition]

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  def getPosition(
      schemaId:   ContainerSchemaId,
      positionId: ContainerSchemaPositionId
    ): DomainValidation[ContainerSchemaPosition]

}

@Singleton
class ContainerSchemaRepositoryImpl @Inject()(val testData: TestData)
    extends ReadWriteRepositoryRefImpl[ContainerSchemaId, ContainerSchema](v => v.id)
    with ContainerSchemaRepository {
  import org.biobank.CommonValidations._

  def nextIdentity: ContainerSchemaId = new ContainerSchemaId(nextIdentityAsString)

  protected def notFound(id: ContainerSchemaId): IdNotFound = IdNotFound(s"container schema id: $id")

  override def init(): Unit = {
    super.init()
    testData.testContainerSchemas.foreach(put)
  }

  def getPosition(schemaId: ContainerSchemaId, label: String): DomainValidation[ContainerSchemaPosition] =
    for {
      schema   <- getByKey(schemaId)
      position <- schema.getPosition(label)
    } yield position

  def getPosition(
      schemaId:   ContainerSchemaId,
      positionId: ContainerSchemaPositionId
    ): DomainValidation[ContainerSchemaPosition] =
    for {
      schema   <- getByKey(schemaId)
      position <- schema.getPosition(positionId)
    } yield position
}
