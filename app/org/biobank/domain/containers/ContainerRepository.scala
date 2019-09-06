package org.biobank.domain.containers

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.centres.CentreId
import scalaz.Scalaz._
import scalaz.Validation.FlatMap._

@ImplementedBy(classOf[ContainerRepositoryImpl])
trait ContainerRepository extends ReadWriteRepositoryWithSlug[ContainerId, Container] {

  def getByInventoryId(inventoryId: String): DomainValidation[Container]

  def getStorageContainer(id: ContainerId): DomainValidation[StorageContainer]

  def getSpecimenContainer(id: ContainerId): DomainValidation[SpecimenContainer]

  def getChildContainer(id: ContainerId, label: String): DomainValidation[Container]

  def positionEmpty(id: ContainerId, label: String): DomainValidation[Unit]

  def containerSharedProperties(ids: ContainerId): ContainerSharedProperties

  def rootContainers(centreId: CentreId): Set[StorageContainer]

  def getRootContainer(id: ContainerId): DomainValidation[RootContainer]

}

@Singleton
class ContainerRepositoryImpl @Inject()(val testData: TestData)
    extends ReadWriteRepositoryRefImplWithSlug[ContainerId, Container](v => v.id) with ContainerRepository {
  import org.biobank.CommonValidations._

  override def init(): Unit = {
    super.init()
    testData.testContainers.foreach(put)
  }

  def nextIdentity: ContainerId = new ContainerId(nextIdentityAsString)

  protected def notFound(id: ContainerId): IdNotFound = IdNotFound(s"container id: $id")

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound =
    EntityCriteriaNotFound(s"container slug: $slug")

  def getByInventoryId(inventoryId: String): DomainValidation[Container] =
    getValues
      .find { c =>
        c.inventoryId == inventoryId
      }.toSuccessNel(EntityCriteriaError(s"container with inventory ID not found: $inventoryId").toString)

  def getStorageContainer(id: ContainerId): DomainValidation[StorageContainer] =
    getByKey(id).flatMap {
      _ match {
        case c: StorageContainer => c.successNel[String]
        case _ => InvalidStatus(s"not a storage container: $id").failureNel[StorageContainer]
      }
    }

  def getSpecimenContainer(id: ContainerId): DomainValidation[SpecimenContainer] =
    getByKey(id).flatMap {
      _ match {
        case c: SpecimenContainer => c.successNel[String]
        case _ => InvalidStatus(s"not a specimen container: $id").failureNel[SpecimenContainer]
      }
    }

  def getChildContainer(id: ContainerId, label: String): DomainValidation[Container] =
    getValues
      .collect { case c: ChildContainer => c }
      .find { c =>
        c.parentId == id && c.schemaLabel.label == label
      }
      .toSuccessNel(EntityCriteriaError(s"container with parent $id and label $label not found").toString)

  def positionEmpty(id: ContainerId, label: String): DomainValidation[Unit] =
    getChildContainer(id, label).fold(
      err => ().successNel[String],
      container =>
        EntityCriteriaError(s"position not empty at label $label in container $id").failureNel[Unit]
    )

  def containerSharedProperties(ids: ContainerId): ContainerSharedProperties =
    ???

  def rootContainers(centreId: CentreId): Set[StorageContainer] =
    ???

  def getRootContainer(id: ContainerId): DomainValidation[RootContainer] = {
    val parent = getByKey(id).flatMap { container =>
      container match {
        case c: ChildContainer => getParent(c)
        case c: RootContainer  => c.successNel[String]
      }
    }
    parent.flatMap { p =>
      p match {
        case c: RootContainer  => c.successNel[String]
        case c: ChildContainer => EntityCriteriaError("parent not found").failureNel[RootContainer]
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion", "org.wartremover.warts.Overloading"))
  private def getParent(container: ChildContainer): DomainValidation[Container] =
    getByKey(container.parentId).flatMap { parent =>
      parent match {
        case c: ChildContainer => getParent(c)
        case c: RootContainer  => c.successNel[String]
      }
    }
}
