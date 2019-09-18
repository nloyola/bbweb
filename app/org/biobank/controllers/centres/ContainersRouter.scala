package org.biobank.controllers.centres

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class ContainersRouter @Inject()(controller: ContainersController) extends SimpleRouter {
  import CentreRouting._
  import org.biobank.controllers.SlugRouting._

  override def routes: Routes = {

    case GET(p"/search/${centreId(cId)}") =>
      // this action extracts parameters from the raw query string
      controller.search(cId)

    case GET(p"/${slug(s)}") =>
      controller.getBySlug(s)

    case GET(p"/children/${slug(s)}") =>
      controller.getChildrenBySlug(s)

    case POST(p"/root") =>
      controller.addRootContainer

    case POST(p"/storage") =>
      controller.addStorageContainer

    case POST(p"/specimen") =>
      controller.addSpecimenContainer

    case POST(p"/label/${containerId(id)}") =>
      controller.updateLabel(id)

    case POST(p"/inventoryId/${containerId(id)}") =>
      controller.updateInventoryId(id)

    case POST(p"/enabled/${containerId(id)}") =>
      controller.updateEnabled(id)

    case POST(p"/containerType/${containerId(id)}") =>
      controller.updateContainerType(id)

    case POST(p"/location/${containerId(id)}") =>
      controller.updateCentreLocation(id)

    case POST(p"/temperature/${containerId(id)}") =>
      controller.updateTemperature(id)

    case POST(p"/constraints/remove/${containerId(id)}") =>
      controller.removeConstraints(id)

    case POST(p"/constraints/${containerId(id)}") =>
      controller.updateConstraints(id)

    case POST(p"/position/${containerId(id)}") =>
      controller.updatePosition(id)

    case POST(p"/snapshot") =>
      controller.snapshot

    case DELETE(p"/${containerId(id)}/${long(ver)}") =>
      controller.remove(id, ver)

  }

}
