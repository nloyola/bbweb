package org.biobank.domain.containers

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.centres.CentreId

@ImplementedBy(classOf[ContainerSchemaRepositoryImpl])
trait ContainerSchemaRepository extends ReadWriteRepositoryWithSlug[ContainerSchemaId, ContainerSchema] {

  def allForCentre(centreId: CentreId): Set[ContainerSchema]

  def isLabelValid(schemaId: ContainerSchemaId, label: String): DomainValidation[Boolean]

}

@Singleton
class ContainerSchemaRepositoryImpl @Inject()(val testData: TestData)
    extends ReadWriteRepositoryRefImplWithSlug[ContainerSchemaId, ContainerSchema](v => v.id)
    with ContainerSchemaRepository {
  import org.biobank.CommonValidations._

  def nextIdentity: ContainerSchemaId = new ContainerSchemaId(nextIdentityAsString)

  protected def notFound(id: ContainerSchemaId): IdNotFound = IdNotFound(s"container schema id: $id")

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound =
    EntityCriteriaNotFound(s"container schema slug: $slug")

  override def init(): Unit = {
    super.init()
    testData.testContainerSchemas.foreach(put)
  }

  def allForCentre(centreId: CentreId): Set[ContainerSchema] =
    getValues.filter { s =>
      s.centreId == centreId || s.shared
    }.toSet

  def isLabelValid(schemaId: ContainerSchemaId, label: String): DomainValidation[Boolean] =
    getByKey(schemaId).map(_.isLabelValid(label))
}
