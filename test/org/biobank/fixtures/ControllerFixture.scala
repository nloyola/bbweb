package org.biobank.fixtures

import com.google.inject.AbstractModule
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.test._
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import net.codingwell.scalaguice.ScalaModule
import org.biobank.Global.{DefaultUserEmail, DefaultUserId}
import org.biobank.controllers.CacheForTesting
import org.biobank._
import org.biobank.domain._
import org.biobank.domain.access._
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.participants._
import org.biobank.domain.processing._
import org.biobank.domain.studies._
import org.biobank.domain.users._
import org.biobank.matchers.ApiResultMatchers
import org.biobank.query.centres._
import org.biobank.services.PasswordHasher
import org.biobank.utils.auth.DefaultEnv
import org.scalatest._
import org.scalatestplus.play._
import org.scalatestplus.play.guice.GuiceOneServerPerTest
import org.slf4j.{Logger, LoggerFactory}
import play.api.cache.{AsyncCacheApi, DefaultSyncCacheApi, SyncCacheApi}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.mvc._
import play.api.test.Helpers._
import play.api.test._
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class Url(val path: String) extends AnyVal {
  override def toString: String = path

  def append(paths: String*): Url = {
    val allPaths = path +: paths
    Url(allPaths.mkString("/"))
  }

  def addQueryString(params: String*): Url =
    Url(path + "?" + params.mkString("&"))
}

object Url {

  def apply(path: String) = new Url(path)

}

class BbwebTestModule extends AbstractModule {

  override def configure() = {
    bind(classOf[Global]).asEagerSingleton
    //bind(classOf[ShipmentsQuery]).asEagerSingleton
  }

}

/**
 * This trait allows a test suite to run tests on a Play Framework fake application.
 *
 * It uses the [[https://github.com/ddevore/akka-persistence-mongo/ Mongo Journal for Akka Persistence]] to
 * make it easier to drop all items in the database prior to running a test in a test suite.
 */
abstract class ControllerFixture
    extends FunSpec with GuiceOneServerPerTest with OneBrowserPerTest with HtmlUnitFactory with BeforeAndAfter
    with MustMatchers with OptionValues with ApiResultMatchers {

  protected val log: Logger = LoggerFactory.getLogger(this.getClass)

  protected val nameGenerator = new NameGenerator(this.getClass())

  protected val factory = new Factory

  private val loginInfo = LoginInfo(CredentialsProvider.ID, DefaultUserId.id)

  private val apiPath = "/api"

  protected val basePath: String

  private val identity =
    org.biobank.utils.auth.User(
      user = RegisteredUser(id = DefaultUserId,
                            version      = 0L,
                            timeAdded    = org.biobank.Global.StartOfTime,
                            timeModified = None,
                            slug         = Slug(DefaultUserEmail),
                            name         = DefaultUserEmail,
                            email        = DefaultUserEmail,
                            password     = "",
                            salt         = "",
                            avatarUrl    = None)
    )

  implicit val env: Environment[DefaultEnv] =
    new FakeEnvironment[DefaultEnv](Seq(loginInfo -> identity))

  /**
   * A fake Guice module.
   */
  class FakeModule extends AbstractModule with ScalaModule {
    override def configure() =
      bind[Environment[DefaultEnv]].toInstance(env)
  }

  override def newAppForTest(testData: org.scalatest.TestData) =
    new GuiceApplicationBuilder()
      .overrides(bind[SyncCacheApi].to[DefaultSyncCacheApi])
      .overrides(bind[AsyncCacheApi].to[CacheForTesting])
      .overrides(new FakeModule)
      .build

  // for the following getters: a new application is created for each test, therefore,
  // new instances of each of these is created with the new application

  protected def passwordHasher = app.injector.instanceOf[PasswordHasher]

  protected def accessItemRepository = app.injector.instanceOf[AccessItemRepository]
  protected def membershipRepository = app.injector.instanceOf[MembershipRepository]

  protected def userRepository = app.injector.instanceOf[UserRepository]

  protected def studyRepository               = app.injector.instanceOf[StudyRepository]
  protected def collectionEventTypeRepository = app.injector.instanceOf[CollectionEventTypeRepository]
  protected def processingTypeRepository      = app.injector.instanceOf[ProcessingTypeRepository]

  protected def participantRepository     = app.injector.instanceOf[ParticipantRepository]
  protected def collectionEventRepository = app.injector.instanceOf[CollectionEventRepository]
  protected def ceventSpecimenRepository  = app.injector.instanceOf[CeventSpecimenRepository]
  protected def specimenRepository        = app.injector.instanceOf[SpecimenRepository]
  protected def processingEventInputSpecimenRepository =
    app.injector.instanceOf[ProcessingEventInputSpecimenRepository]

  protected def centreRepository            = app.injector.instanceOf[CentreRepository]
  protected def shipmentsWriteRepository    = app.injector.instanceOf[ShipmentsWriteRepository]
  protected def shipmentSpecimensRepository = app.injector.instanceOf[ShipmentSpecimensWriteRepository]
  protected def containerTypeRepository     = app.injector.instanceOf[ContainerTypeRepository]
  protected def containerSchemaRepository   = app.injector.instanceOf[ContainerSchemaRepository]
  protected def containerRepository         = app.injector.instanceOf[ContainerRepository]

  protected def shipmentsReadRepository         = app.injector.instanceOf[ShipmentsReadRepository]
  protected def shipmentSpecimensReadRepository = app.injector.instanceOf[ShipmentSpecimensReadRepository]

  protected def addToRepository[T <: ConcurrencySafeEntity[_]](entity: T): Unit =
    entity match {
      case e: AccessItem          => accessItemRepository.put(e)
      case e: Membership          => membershipRepository.put(e)
      case e: User                => userRepository.put(e)
      case e: Study               => studyRepository.put(e)
      case e: Centre              => centreRepository.put(e)
      case e: CollectionEventType => collectionEventTypeRepository.put(e)
      case e: ProcessingType      => processingTypeRepository.put(e)
      case e: Participant         => participantRepository.put(e)
      case e: CollectionEvent     => collectionEventRepository.put(e)
      case e: Specimen            => specimenRepository.put(e)
      case e: ShipmentSpecimen    => shipmentSpecimensRepository.put(e)
      case e: Shipment            => shipmentsWriteRepository.put(e)
      case e: ContainerSchema     => containerSchemaRepository.put(e)
      case e: ContainerType       => containerTypeRepository.put(e)
      case e: Container           => containerRepository.put(e)
      case _ => fail("invalid entity")
    }

  protected def addAllToRepository(entities: Set[ConcurrencySafeEntity[_]]): Unit =
    entities.foreach(addToRepository)

  protected def makeAuthRequest(method: String, url: Url, json: JsValue = JsNull): Option[Future[Result]] = {
    val fakeRequest = FakeRequest(method, url.path)
      .withJsonBody(json)
      .withAuthenticator(loginInfo)

    if (json != JsNull) {
      log.debug(s"request: ${method}, ${url.path},\n${Json.prettyPrint(json)}")
    } else {
      log.debug(s"request: ${method}, ${url.path}")
    }

    route(app, fakeRequest)
  }

  protected def uri(paths: String*): Url = {
    val prefix   = if (basePath.isEmpty()) Array(apiPath) else Array(apiPath, basePath)
    val allPaths = prefix ++ paths
    Url(allPaths.mkString("/"))
  }

}
