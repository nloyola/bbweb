package org.biobank.query.db

import cats.data._
import cats.implicits._
import com.google.inject.ImplementedBy
import javax.inject.Inject
import org.biobank.validation.Validation._
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.{ExecutionContext, Future}
import scalaz.Scalaz._
import org.slf4j.LoggerFactory

@ImplementedBy(classOf[SequenceNumbersDaoSlick])
trait SequenceNumbersDao {

  def insertOrUpdate(sequenceNumber: SequenceNumber): Future[Unit]

  def remove(persistenceId: String): Future[Unit]

  def sequenceNumberForId(persistenceId: String): EitherT[Future, ValidationError, SequenceNumber]

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

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def insertOrUpdate(sequenceNumber: SequenceNumber): Future[Unit] = {
    db.run(sequenceNumbers.insertOrUpdate(sequenceNumber).map(_ => ()))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def remove(persistenceId: String): Future[Unit] = {
    val query  = sequenceNumbers.filter(s => s.persistenceId === persistenceId)
    val action = query.delete
    db.run(action.map(_ => ()))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def sequenceNumberForId(persistenceId: String): EitherT[Future, ValidationError, SequenceNumber] = {
    val f = db.run(sequenceNumbers.filter(s => s.persistenceId === persistenceId).result.headOption).map {
      _ match {
        case Some(pid) => Either.right(pid)
        case None      => Either.left(IllegalStateError(s"persistence ID not found: $persistenceId"))

      }
    }
    EitherT(f)
  }

}
