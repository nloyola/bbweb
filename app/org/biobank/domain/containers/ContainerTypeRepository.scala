package org.biobank.domain.containers

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerTypeRepositoryImpl])
trait ContainerTypeRepository extends ReadWriteRepositoryWithSlug[ContainerTypeId, ContainerType] {

  def getStorageContainerType(id: ContainerTypeId): DomainValidation[StorageContainerType]

  def getSpecimenContainerType(id: ContainerTypeId): DomainValidation[SpecimenContainerType]

  def allForCentre(centreId: CentreId): Set[ContainerType]

  def schemaInUse(schemaId: ContainerSchemaId): Boolean

}

@Singleton
class ContainerTypeRepositoryImpl @Inject()(val testData: TestData)
    extends ReadWriteRepositoryRefImplWithSlug[ContainerTypeId, ContainerType](v => v.id)
    with ContainerTypeRepository {
  import org.biobank.CommonValidations._

  override def init(): Unit = {
    super.init()
    //testData.testContainerTypes.foreach(put)
  }

  def nextIdentity: ContainerTypeId = new ContainerTypeId(nextIdentityAsString)

  protected def notFound(id: ContainerTypeId): IdNotFound = IdNotFound(s"container type id: $id")

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound =
    EntityCriteriaNotFound(s"container type slug: $slug")

  def getStorageContainerType(id: ContainerTypeId): DomainValidation[StorageContainerType] =
    for {
      ct <- getByKey(id)
      storageCt <- ct match {
                    case s: StorageContainerType => s.successNel[String]
                    case _ =>
                      InvalidStatus(s"not a storage container type: $id").failureNel[StorageContainerType]
                  }
    } yield storageCt

  def getSpecimenContainerType(id: ContainerTypeId): DomainValidation[SpecimenContainerType] =
    for {
      ct <- getByKey(id)
      specimenCt <- ct match {
                     case s: SpecimenContainerType => s.successNel[String]
                     case _ =>
                       InvalidStatus(s"not a specimen container type: $id").failureNel[SpecimenContainerType]
                   }
    } yield specimenCt

  def allForCentre(centreId: CentreId): Set[ContainerType] =
    getValues.filter { ct =>
      (ct.centreId == centreId) || ct.shared
    }.toSet

  def schemaInUse(schemaId: ContainerSchemaId): Boolean =
    getValues.exists { ct =>
      (ct.schemaId == schemaId)
    }

}
