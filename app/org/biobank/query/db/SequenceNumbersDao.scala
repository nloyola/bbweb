package org.biobank.query.db

import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank._
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
import scalaz._
import org.slf4j.LoggerFactory

@ImplementedBy(classOf[SequenceNumbersDaoSlick])
trait SequenceNumbersDao {

  def insertOrUpdate(sequenceNumber: SequenceNumber): Future[Unit]

  def remove(persistenceId: String): Future[Unit]

  def sequenceNumberForId(persistenceId: String): FutureValidation[SequenceNumber]

}

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
class SequenceNumbersDaoSlick @Inject()(
    protected val dbConfigProvider: DatabaseConfigProvider
  )(
    implicit
    val ec: ExecutionContext)
    extends SequenceNumbersDao with DatabaseSchema {
  import dbConfig.profile.api._

  protected val log = LoggerFactory.getLogger(this.getClass)

  def insertOrUpdate(sequenceNumber: SequenceNumber): Future[Unit] = {
    db.run(sequenceNumbers.insertOrUpdate(sequenceNumber).map(_ => ()))
  }

  def remove(persistenceId: String): Future[Unit] = {
    val query  = sequenceNumbers.filter(s => s.persistenceId === persistenceId)
    val action = query.delete
    db.run(action.map(_ => ()))
  }

  def sequenceNumberForId(persistenceId: String): FutureValidation[SequenceNumber] = {
    FutureValidation(
    db.run(sequenceNumbers.filter(s => s.persistenceId === persistenceId).result.headOption)
      .map(_.toSuccessNel(s"persistence ID not found: $persistenceId"))
    )
  }

}
