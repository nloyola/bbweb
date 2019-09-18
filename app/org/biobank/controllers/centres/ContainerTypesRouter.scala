package org.biobank.controllers.centres

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class ContainerTypesRouter @Inject()(controller: ContainerTypesController) extends SimpleRouter {
  import CentreRouting._
  import org.biobank.controllers.SlugRouting._

  override def routes: Routes = {

    case GET(p"/search/${centreId(id)}") =>
      // this action extracts parameters from the raw query string
      controller.search(id)

    case GET(p"/${slug(s)}") =>
      controller.getBySlug(s)

    case POST(p"/storage") =>
      controller.addStorage

    case POST(p"/specimen") =>
      controller.addSpecimen

    case POST(p"/name/${containerTypeId(id)}") =>
      controller.updateName(id)

    case POST(p"/description/${containerTypeId(id)}") =>
      controller.updateDescription(id)

    case POST(p"/centre/${containerTypeId(id)}") =>
      controller.updateCentre(id)

    case POST(p"/schema/${containerTypeId(id)}") =>
      controller.updateSchema(id)

    case POST(p"/shared/${containerTypeId(id)}") =>
      controller.updateShared(id)

    case POST(p"/enabled/${containerTypeId(id)}") =>
      controller.updateEnabled(id)

    case POST(p"/snapshot") =>
      controller.snapshot

    case DELETE(p"/${containerTypeId(id)}/${long(ver)}") =>
      controller.remove(id, ver)

  }

}
