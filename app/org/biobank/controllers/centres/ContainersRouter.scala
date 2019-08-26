package org.biobank.controllers.centres

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class ContainersRouter @Inject()(controller: ContainersController) extends SimpleRouter {
  import CentreRouting._
  import org.biobank.controllers.SlugRouting._

  override def routes: Routes = {

    case GET(p"/search") =>
      // this action extracts parameters from the raw query string
      controller.list

    case GET(p"/${slug(s)}") =>
      controller.getBySlug(s)

    case POST(p"/") =>
      controller.addRootContainer

    case POST(p"/add-storage") =>
      controller.addStorageContainer

    case POST(p"/add-specimen") =>
      controller.addStorageContainer

    case POST(p"/snapshot") =>
      controller.snapshot

    case POST(p"/update/${containerId(id)}") =>
      controller.update(id)

  }

}
