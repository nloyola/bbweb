package org.biobank.domain.participants

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import org.biobank.domain.containers.ContainerId
import scalaz.Scalaz._

@ImplementedBy(classOf[SpecimenRepositoryImpl])
trait SpecimenRepository extends ReadWriteRepositoryWithSlug[SpecimenId, Specimen] {

  def getByInventoryId(inventoryId: String): DomainValidation[Specimen]

  /**
   * Ensures that the inventory IDs are for specimens already in the system.
   *
   * @param specimenInventoyIds one or more inventory IDs.
   *
   * @return a validation: Success if all inventory IDS match specimens in the system. Failure if not.
   */
  def getByInventoryIds(specimenInventoryIds: String*): DomainValidation[List[Specimen]]

  def containerInUse(containerId: ContainerId): Boolean

}

@Singleton
class SpecimenRepositoryImpl @Inject()(val testData: TestData)
    extends StmReadWriteRepositoryImplWithSlug[SpecimenId, Specimen](v => v.id) with SpecimenRepository {
  import org.biobank.CommonValidations._

  override def init(): Unit = {
    super.init()
    testData.testSpecimens.foreach(put)
  }

  def nextIdentity: SpecimenId = new SpecimenId(nextIdentityAsString)

  protected def notFound(id: SpecimenId): IdNotFound = IdNotFound(s"specimen id: $id")

  protected def slugNotFound(slug: Slug): EntityCriteriaNotFound =
    EntityCriteriaNotFound(s"specimen slug: $slug")

  def inventoryIdCriteriaError(inventoryId: String): String =
    IdNotFound(s"specimen inventory ID: $inventoryId").toString

  def getByInventoryId(inventoryId: String): DomainValidation[Specimen] =
    getValues
      .find(s => s.inventoryId == inventoryId)
      .toSuccessNel(inventoryIdCriteriaError(inventoryId))

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getByInventoryIds(specimenInventoryIds: String*): DomainValidation[List[Specimen]] = {
    val inventoryIds = specimenInventoryIds.map(_.trim)
    inventoryIds
      .map { inventoryId =>
        getByInventoryId(inventoryId).fold(err => inventoryId.failureNel[Specimen],
                                           spc => spc.successNel[String])
      }
      .toList.sequenceU
      .leftMap(
        err =>
          EntityCriteriaNotFound { s"invalid specimen inventory IDs: " + err.list.toList.mkString(", ") }.nel
      )
  }

  def containerInUse(containerId: ContainerId): Boolean =
    getValues.exists(s => s.containerId == Some(containerId))

}
