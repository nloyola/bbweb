package org.biobank.controllers.participants

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class SpecimensRouter @Inject()(controller: SpecimensController) extends SimpleRouter {
  import ParticipantsRouting._
  import org.biobank.controllers.SlugRouting._

  override def routes: Routes = {
    case GET(p"/get/${slug(s)}") =>
      controller.get(s)

    case GET(p"/${slug(s)}") =>
      // this action extracts parameters from the query string
      controller.list(s)

    case POST(p"/snapshot") =>
      controller.snapshot

    case POST(p"/${collectionEventId(ceId)}") =>
      controller.addSpecimens(ceId)

    case DELETE(p"/${collectionEventId(ceId)}/${specimenId(spcId)}/${long(ver)}") =>
      controller.removeSpecimen(ceId, spcId, ver)

  }
}
