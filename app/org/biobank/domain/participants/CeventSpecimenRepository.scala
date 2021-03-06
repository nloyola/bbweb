package org.biobank.domain.participants

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import org.biobank.TestData
import org.biobank.domain._
import scalaz.Scalaz._

/**
 *
 */
@ImplementedBy(classOf[CeventSpecimenRepositoryImpl])
trait CeventSpecimenRepository extends ReadWriteRepository[SpecimenId, CeventSpecimen] {

  def withCeventId(ceventId: CollectionEventId): Set[CeventSpecimen]

  def withSpecimenId(specimenId: SpecimenId): DomainValidation[CeventSpecimen]

}

@Singleton
class CeventSpecimenRepositoryImpl @Inject()(val testData: TestData)
    extends StmReadWriteRepositoryImpl[SpecimenId, CeventSpecimen](v => v.specimenId)
    with CeventSpecimenRepository {

  import org.biobank.CommonValidations._

  // only existing collection event and specimen IDs should be stored, never new IDs
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def nextIdentity: SpecimenId = throw new IllegalStateException("should not be used")

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  protected def notFound(id: SpecimenId): IdNotFound =
    IdNotFound(s"specimen with ID: $id")

  override def init(): Unit = {
    super.init()
    testData.testCeventSpecimens.foreach(put)
  }

  def withCeventId(ceventId: CollectionEventId): Set[CeventSpecimen] =
    getValues.filter(x => x.ceventId == ceventId).toSet

  def withSpecimenId(specimenId: SpecimenId): DomainValidation[CeventSpecimen] = {
    val ceventSpecimens = getValues.filter(x => x.specimenId == specimenId).toSet
    if (ceventSpecimens.isEmpty) {
      IdNotFound(s"cevent specimen repository: specimen id: ${specimenId.id}")
        .failureNel[CeventSpecimen]
    } else if (ceventSpecimens.size > 1) {
      DomainError(s"cevent specimen repository: more than one entry found for scpecimen: ${specimenId.id}")
        .failureNel[CeventSpecimen]
    } else {
      ceventSpecimens.headOption.toSuccessNel("list not expected to be empty")
    }
  }

}
