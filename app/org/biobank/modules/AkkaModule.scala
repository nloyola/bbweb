package org.biobank.modules

import com.google.inject.AbstractModule
import org.biobank.services.access._
import org.biobank.services.centres._
import org.biobank.services.participants._
import org.biobank.services.studies._
import org.biobank.services.users._
import play.api.libs.concurrent.AkkaGuiceSupport

class AkkaModule extends AbstractModule with AkkaGuiceSupport {

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  override def configure() = {

    bindActor[AccessProcessor]("accessProcessor")
    bindActor[MembershipProcessor]("membershipProcessor")
    bindActor[UsersProcessor]("usersProcessor")
    bindActor[CentresProcessor]("centresProcessor")
    bindActor[ContainersProcessor]("containersProcessor")
    bindActor[ContainerTypesProcessor]("containerTypesProcessor")
    bindActor[ContainerSchemasProcessor]("containerSchemasProcessor")
    bindActor[ShipmentsProcessor]("shipmentsProcessor")

    bindActor[ParticipantsProcessor]("participantsProcessor")
    bindActor[CollectionEventsProcessor]("collectionEventsProcessor")
    bindActor[SpecimensProcessor]("specimensProcessor")

    bindActor[StudiesProcessor]("studiesProcessor")
    bindActor[CollectionEventTypeProcessor]("collectionEventType")
    bindActor[ProcessingTypeProcessor]("processingType")
  }

}
