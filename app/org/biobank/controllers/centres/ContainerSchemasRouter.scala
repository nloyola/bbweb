package org.biobank.controllers.centres

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class ContainerSchemasRouter @Inject()(controller: ContainerSchemasController) extends SimpleRouter {
  import CentreRouting._
  import org.biobank.controllers.SlugRouting._

  override def routes: Routes = {

    case GET(p"/search/${centreId(id)}") =>
      // this action extracts parameters from the raw query string
      controller.search(id)

    case GET(p"/${slug(s)}") =>
      controller.getBySlug(s)

    case POST(p"/") =>
      controller.addSchema

    case POST(p"/name/${containerSchemaId(id)}") =>
      controller.updateName(id)

    case POST(p"/description/${containerSchemaId(id)}") =>
      controller.updateDescription(id)

    case POST(p"/shared/${containerSchemaId(id)}") =>
      controller.updateShared(id)

    case POST(p"/centre/${containerSchemaId(id)}") =>
      controller.updateCentre(id)

    case POST(p"/labels/${containerSchemaId(id)}") =>
      controller.updateLabels(id)

    case POST(p"/snapshot") =>
      controller.snapshot

    case DELETE(p"/${containerSchemaId(id)}/${long(ver)}") =>
      controller.remove(id, ver)

  }

}
