package org.biobank.controllers.centres

//import java.time.OffsetDateTime
import org.biobank.controllers.PagedResultsSharedSpec
//import org.biobank.domain.{Location, Slug}
//import org.biobank.domain.containers._
//import org.biobank.domain.studies.{Study, StudyId}
//import org.biobank.dto.{ContainerDto, EntityInfoAndStateDto}
import org.biobank.fixtures.{ControllerFixture} //, Url}
import org.biobank.matchers.PagedResultsMatchers
//import org.scalatest.prop.TableDrivenPropertyChecks._
//import play.api.libs.json._
//import play.api.test.Helpers._

/**
  * Tests the REST API for [[Container]]s.
  */
class ContainersControllerSpec
    extends ControllerFixture
    with PagedResultsSharedSpec
    with PagedResultsMatchers {

  //import org.biobank.TestUtils._
  //import org.biobank.matchers.JsonMatchers._
  //import org.biobank.matchers.DtoMatchers._
  //import org.biobank.matchers.EntityMatchers._

  protected val basePath = "centre/containers"

  //private def uri(container: Container): Url = uri(container.id.id)

  //private def uri(container: Container, path: String): Url = uri(path, container.id.id)

}
