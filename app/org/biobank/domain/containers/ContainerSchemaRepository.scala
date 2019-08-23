package org.biobank.domain.containers

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
//import org.biobank.domain.centres.CentreId
//import scalaz.Scalaz._
//import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerSchemaRepositoryImpl])
trait ContainerSchemaRepository extends ReadWriteRepository[ContainerSchemaId, ContainerSchema] {}

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
}
