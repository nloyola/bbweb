package org.biobank.domain.containers

import com.google.inject.ImplementedBy
import javax.inject.{Inject , Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerRepositoryImpl])
trait ContainerRepository extends ReadWriteRepositoryWithSlug[ContainerId, Container] {

  def getStorageContainer(id: ContainerId): DomainValidation[StorageContainer]

  def getSpecimenContainer(id: ContainerId): DomainValidation[SpecimenContainer]

  def containerSharedProperties(ids: ContainerId): ContainerSharedProperties

  def rootContainers(centreId: CentreId): Set[StorageContainer]

  def getSubContainerCentre(id: ContainerId): DomainValidation[CentreId]

}

@Singleton
class ContainerRepositoryImpl @Inject() (val testData: TestData)
    extends ReadWriteRepositoryRefImplWithSlug[ContainerId, Container](v => v.id)
    with ContainerRepository {
  import org.biobank.CommonValidations._

  override def init(): Unit = {
    super.init()
    testData.testContainers.foreach(put)
  }

  def nextIdentity: ContainerId = new ContainerId(nextIdentityAsString)

  protected def notFound(id: ContainerId): IdNotFound = IdNotFound(s"container id: $id")

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound =
    EntityCriteriaNotFound(s"container slug: $slug")

  def getStorageContainer(id: ContainerId): DomainValidation[StorageContainer] = {
   for {
      container <- getByKey(id)
      storage <- container match {
        case c: StorageContainer => c.successNel[String]
        case _ => InvalidStatus(s"not a storage container: $id").failureNel[StorageContainer]
      }
    } yield storage
   }

  def getSpecimenContainer(id: ContainerId): DomainValidation[SpecimenContainer] = {
   for {
      container <- getByKey(id)
      sc <- container match {
        case c: SpecimenContainer => c.successNel[String]
        case _ => InvalidStatus(s"not a specimen container: $id").failureNel[SpecimenContainer]
      }
    } yield sc
  }

  def containerSharedProperties(ids: ContainerId): ContainerSharedProperties = {
    ???
  }

  def rootContainers(centreId: CentreId): Set[StorageContainer] = {
    ???
  }

  def getSubContainerCentre(id: ContainerId): DomainValidation[CentreId] = {
    ???
  }
}
