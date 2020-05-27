package org.biobank.domain.processing

import org.biobank.domain.{ReadWriteRepository, StmReadWriteRepositoryImpl}
import com.google.inject.ImplementedBy
import javax.inject.Singleton
import org.biobank.domain.participants.SpecimenId

/**
 * A record for when a specimen was processed.
 */
@ImplementedBy(classOf[ProcessingEventInputSpecimenRepositoryImpl])
trait ProcessingEventInputSpecimenRepository
    extends ReadWriteRepository[ProcessingEventInputSpecimenId, ProcessingEventInputSpecimen] {

  def withProcessingEventId(processingEventId: ProcessingEventId): Set[ProcessingEventInputSpecimen]

  def withSpecimenId(specimenId: SpecimenId): Set[ProcessingEventInputSpecimen]

}

@Singleton
class ProcessingEventInputSpecimenRepositoryImpl
    extends StmReadWriteRepositoryImpl[ProcessingEventInputSpecimenId, ProcessingEventInputSpecimen](
      v => v.id
    ) with ProcessingEventInputSpecimenRepository {

  import org.biobank.CommonValidations._

  def nextIdentity(): ProcessingEventInputSpecimenId =
    new ProcessingEventInputSpecimenId(nextIdentityAsString)

  protected def notFound(id: ProcessingEventInputSpecimenId): IdNotFound =
    IdNotFound(s"processing event input specimen type: $id")

  def withProcessingEventId(processingEventId: ProcessingEventId): Set[ProcessingEventInputSpecimen] =
    getValues.filter(p => p.processingEventId == processingEventId).toSet

  def withSpecimenId(specimenId: SpecimenId): Set[ProcessingEventInputSpecimen] =
    getValues.filter(p => p.specimenId == specimenId).toSet

}
