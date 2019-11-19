package org.biobank

import akka.stream.Materializer
import java.time.{OffsetDateTime, ZoneOffset}
import javax.inject._
import org.biobank.domain.Slug
import org.biobank.domain.users._
import org.biobank.query.db.DatabaseSchema
import play.api.{Configuration, Logger}
import scala.concurrent.ExecutionContext
//import scala.util.{Failure, Success}
import play.api.db.slick.DatabaseConfigProvider

/**
 * This is a trait so that it can be used by tests also.
 */
@Singleton
@SuppressWarnings(
  Array("org.wartremover.warts.Throw",
        "org.wartremover.warts.ImplicitParameter",
        "org.wartremover.warts.NonUnitStatements")
)
class Global @Inject()(
    configuration:                  Configuration,
    protected val dbConfigProvider: DatabaseConfigProvider
  )(
    implicit
    val mat: Materializer,
    val ec:  ExecutionContext)
    extends DatabaseSchema {
  import dbConfig.profile.api._

  val log: Logger = Logger(this.getClass)

  def checkConfig(): Unit = {
    if (configuration.get[String]("play.mailer.host").isEmpty) {
      throw new RuntimeException("smtp server information needs to be set in email.conf")
    }

    if (configuration.get[String]("admin.email").isEmpty) {
      throw new RuntimeException("administrator email needs to be set in application.conf")
    }

    val adminUrl = configuration.get[String]("admin.url")
    if (adminUrl.isEmpty) {
      throw new RuntimeException("administrator url needs to be set in application.conf")
    }
  }

  def showSchema(): Unit = {
    if (configuration.get[Boolean]("application.schema.show")) {
      val schema = sequenceNumbers.schema ++ shipments.schema
      schema.create.statements.foreach(s => log.info(s))
      ()
    }
  }

  checkConfig
  showSchema
}

object Global {

  val DefaultUserEmail: String = "admin@admin.com"

  val DefaultUserId: UserId = UserId(Slug.slugify(DefaultUserEmail))

  val StartOfTime: OffsetDateTime = OffsetDateTime.of(1, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC)

  val EndOfTime: OffsetDateTime = OffsetDateTime.of(9999, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC)

}
