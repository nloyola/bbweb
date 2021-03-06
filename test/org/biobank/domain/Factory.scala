package org.biobank.domain

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import org.biobank.domain.access._
import org.biobank.domain.annotations.AnnotationValueType._
import org.biobank.domain.annotations._
import org.biobank.domain.centres._
import org.biobank.domain.containers._
import org.biobank.domain.participants._
import org.biobank.domain.studies._
import org.biobank.domain.users._
import org.biobank.fixtures.NameGenerator
import org.scalatest.OptionValues._
import scala.reflect._

/**
 * This factory class creates domain entities that can be used in test cases.
 *
 * The factory remembers the previously created domain etities. Entities of each type are cached, but only the
 * last one created.
 *
 * If an entity has a dependency on another, the other is created first, or if the other entity has
 * already been created it will be used.  For example, if a participant is created, it will belong to the last
 * study that was created.x
 *
 */
class Factory {

  //private val log = LoggerFactory.getLogger(this.getClass)

  private val nameGenerator = new NameGenerator(this.getClass)

  private var domainObjects: Map[Class[_], _] = Map.empty

  private def nextIdentityAsString[T: ClassTag](): String = Slug.slugify(nameGenerator.next[T])

  def createRegisteredUser(): RegisteredUser = {
    val name = faker.Name.name
    val user = RegisteredUser(version = 0L,
                              timeAdded    = OffsetDateTime.now,
                              timeModified = None,
                              slug         = Slug(name),
                              name         = name,
                              email        = nameGenerator.nextEmail[User],
                              id           = UserId(nextIdentityAsString[User]),
                              password     = nameGenerator.next[User],
                              salt         = nameGenerator.next[User],
                              avatarUrl    = Some(nameGenerator.nextUrl[User]))
    domainObjects = domainObjects + (classOf[RegisteredUser] -> user)
    user
  }

  def createActiveUser: ActiveUser = {
    val name = faker.Name.name
    val user = ActiveUser(version = 0L,
                          timeAdded    = OffsetDateTime.now,
                          timeModified = None,
                          slug         = Slug(name),
                          name         = name,
                          email        = nameGenerator.nextEmail[User],
                          id           = UserId(nextIdentityAsString[User]),
                          password     = nameGenerator.next[User],
                          salt         = nameGenerator.next[User],
                          avatarUrl    = Some(nameGenerator.nextUrl[User]))
    domainObjects = domainObjects + (classOf[ActiveUser] -> user)
    user
  }

  def createLockedUser(): LockedUser = {
    val name = faker.Name.name
    val user = LockedUser(version = 0L,
                          timeAdded    = OffsetDateTime.now,
                          timeModified = None,
                          slug         = Slug(name),
                          name         = name,
                          email        = nameGenerator.nextEmail[User],
                          id           = UserId(nextIdentityAsString[User]),
                          password     = nameGenerator.next[User],
                          salt         = nameGenerator.next[User],
                          avatarUrl    = Some(nameGenerator.nextUrl[User]))
    domainObjects = domainObjects + (classOf[LockedUser] -> user)
    user
  }

  def createRole(): Role = {
    val name = faker.Lorem.sentence(3)
    val role = Role(id = AccessItemId(nextIdentityAsString[AccessItem]),
                    version      = 0L,
                    timeAdded    = OffsetDateTime.now,
                    timeModified = None,
                    slug         = Slug(name),
                    name         = name,
                    description  = Some(nameGenerator.next[Role]),
                    userIds      = Set.empty[UserId],
                    parentIds    = Set.empty[AccessItemId],
                    childrenIds  = Set.empty[AccessItemId])
    domainObjects = domainObjects + (classOf[Role] -> role)
    role
  }

  def createPermission(): Permission = {
    val name = faker.Lorem.sentence(3)
    val permission = Permission(id = AccessItemId(nextIdentityAsString[AccessItem]),
                                version      = 0L,
                                timeAdded    = OffsetDateTime.now,
                                timeModified = None,
                                slug         = Slug(name),
                                name         = name,
                                description  = Some(nameGenerator.next[Permission]),
                                parentIds    = Set.empty[AccessItemId],
                                childrenIds  = Set.empty[AccessItemId])
    domainObjects = domainObjects + (classOf[Permission] -> permission)
    permission
  }

  def createMembership(): Membership = {
    val name = faker.Lorem.sentence(3)
    val membership = Membership(id = MembershipId(nextIdentityAsString[MembershipId]),
                                version      = 0L,
                                timeAdded    = OffsetDateTime.now,
                                timeModified = None,
                                slug         = Slug(name),
                                name         = name,
                                description  = Some(nameGenerator.next[Membership]),
                                userIds      = Set.empty[UserId],
                                studyData    = MembershipEntitySet(false, Set.empty[StudyId]),
                                centreData   = MembershipEntitySet(false, Set.empty[CentreId]))
    domainObjects = domainObjects + (classOf[Membership] -> membership)
    membership
  }

  def createDisabledStudy(): DisabledStudy = {
    val name = faker.Lorem.sentence(3)
    val study = DisabledStudy(version = 0L,
                              timeAdded       = OffsetDateTime.now,
                              timeModified    = None,
                              id              = StudyId(nextIdentityAsString[Study]),
                              slug            = Slug(name),
                              name            = name,
                              description     = Some(nameGenerator.next[Study]),
                              annotationTypes = Set.empty)
    domainObjects = domainObjects + (classOf[DisabledStudy] -> study)
    study
  }

  def createEnabledStudy(): EnabledStudy = {
    val name = faker.Lorem.sentence(3)
    val enabledStudy = EnabledStudy(id = StudyId(nextIdentityAsString[Study]),
                                    version         = 0L,
                                    timeAdded       = OffsetDateTime.now,
                                    timeModified    = None,
                                    slug            = Slug(name),
                                    name            = name,
                                    description     = Some(nameGenerator.next[Study]),
                                    annotationTypes = Set.empty)
    domainObjects = domainObjects + (classOf[EnabledStudy] -> enabledStudy)
    enabledStudy
  }

  def createRetiredStudy(): RetiredStudy = {
    val name = faker.Lorem.sentence(3)
    val retiredStudy = RetiredStudy(id = StudyId(nextIdentityAsString[Study]),
                                    version         = 0L,
                                    timeAdded       = OffsetDateTime.now,
                                    timeModified    = None,
                                    slug            = Slug(name),
                                    name            = name,
                                    description     = Some(nameGenerator.next[Study]),
                                    annotationTypes = Set.empty)
    domainObjects = domainObjects + (classOf[RetiredStudy] -> retiredStudy)
    retiredStudy
  }

  def createCollectedSpecimenDefinition(): CollectedSpecimenDefinition = {
    val name = faker.Lorem.sentence(3)
    val specimenSpec = CollectedSpecimenDefinition(
      id                      = SpecimenDefinitionId(nextIdentityAsString[CollectedSpecimenDefinition]),
      slug                    = Slug(name),
      name                    = name,
      description             = Some(nameGenerator.next[CollectedSpecimenDefinition]),
      units                   = nameGenerator.next[String],
      anatomicalSourceType    = AnatomicalSourceType.Blood,
      preservationType        = PreservationType.FreshSpecimen,
      preservationTemperature = PreservationTemperature.Minus80celcius,
      specimenType            = SpecimenType.FilteredUrine,
      maxCount                = 1,
      amount                  = BigDecimal(0.5)
    )
    domainObjects = domainObjects + (classOf[CollectedSpecimenDefinition] -> specimenSpec)
    specimenSpec
  }

  def createProcessedSpecimenDefinition(): ProcessedSpecimenDefinition = {
    val name = faker.Lorem.sentence(3)
    val specimenSpec = ProcessedSpecimenDefinition(
      id                      = SpecimenDefinitionId(nextIdentityAsString[ProcessedSpecimenDefinition]),
      slug                    = Slug(name),
      name                    = name,
      description             = Some(nameGenerator.next[ProcessedSpecimenDefinition]),
      units                   = nameGenerator.next[String],
      anatomicalSourceType    = AnatomicalSourceType.Blood,
      preservationType        = PreservationType.FreshSpecimen,
      preservationTemperature = PreservationTemperature.Minus80celcius,
      specimenType            = SpecimenType.FilteredUrine
    )
    domainObjects = domainObjects + (classOf[ProcessedSpecimenDefinition] -> specimenSpec)
    specimenSpec
  }

  def createCollectionEventType(): CollectionEventType = {
    val disabledStudy = defaultDisabledStudy
    val name          = faker.Lorem.sentence(3)
    val ceventType = CollectionEventType(
      id                  = CollectionEventTypeId(nextIdentityAsString[CollectionEventType]),
      studyId             = disabledStudy.id,
      version             = 0L,
      timeAdded           = OffsetDateTime.now,
      timeModified        = None,
      slug                = Slug(name),
      name                = name,
      description         = Some(nameGenerator.next[CollectionEventType]),
      recurring           = false,
      specimenDefinitions = Set.empty,
      annotationTypes     = Set.empty
    )

    domainObjects = domainObjects + (classOf[CollectionEventType] -> ceventType)
    ceventType
  }

  def createAnnotationType(
      valueType:     AnnotationValueType,
      maxValueCount: Option[Int],
      options:       Seq[String]
    ): AnnotationType = {
    val name = faker.Lorem.sentence(3)
    val annotationType = AnnotationType(AnnotationTypeId(nameGenerator.next[AnnotationType]),
                                        Slug(name),
                                        name,
                                        None,
                                        valueType,
                                        maxValueCount,
                                        options,
                                        false)

    domainObjects = domainObjects + (classOf[AnnotationType] -> annotationType)
    annotationType
  }

  def createAnnotationType(valueType: AnnotationValueType): AnnotationType =
    createAnnotationType(valueType, None, Seq.empty)

  def createAnnotationType(): AnnotationType =
    createAnnotationType(AnnotationValueType.Text, None, Seq.empty)

  def createProcessingType(): ProcessingType = {
    val disabledStudy = defaultDisabledStudy
    val name          = faker.Lorem.sentence(2)

    val input = InputSpecimenProcessing(expectedChange = BigDecimal(1.0),
                                        count                = 1,
                                        containerTypeId      = None,
                                        definitionType       = ProcessingType.collectedDefinition,
                                        entityId             = "",
                                        specimenDefinitionId = SpecimenDefinitionId(""))
    val output = OutputSpecimenProcessing(expectedChange = BigDecimal(1.0),
                                          count              = 1,
                                          containerTypeId    = None,
                                          specimenDefinition = defaultProcessedSpecimenDefinition)

    val processingType = ProcessingType(id = ProcessingTypeId(nextIdentityAsString[ProcessingType]),
                                        studyId         = disabledStudy.id,
                                        version         = 0L,
                                        timeAdded       = OffsetDateTime.now,
                                        timeModified    = None,
                                        slug            = Slug(name),
                                        name            = name,
                                        description     = Some(faker.Lorem.sentence(4)),
                                        enabled         = false,
                                        input           = input,
                                        output          = output,
                                        annotationTypes = Set.empty,
                                        inUse           = false)

    domainObjects = domainObjects + (classOf[ProcessingType] -> processingType)
    processingType
  }

  def createParticipant(): Participant = {
    val study    = defaultEnabledStudy
    val uniqueId = nextIdentityAsString[Participant]
    val participant = Participant(studyId = study.id,
                                  id           = ParticipantId(nextIdentityAsString[Participant]),
                                  version      = 0L,
                                  timeAdded    = OffsetDateTime.now,
                                  timeModified = None,
                                  slug         = Slug(uniqueId),
                                  uniqueId     = uniqueId,
                                  annotations  = Set.empty)
    domainObjects = domainObjects + (classOf[Participant] -> participant)
    participant
  }

  def createAnnotationValues(
      annotationType: AnnotationType
    ): Tuple3[Option[String], Option[String], Set[String]] =
    annotationType.valueType match {
      case Text =>
        (Some(nameGenerator.next[Annotation]), None, Set.empty)
      case Number =>
        (None, Some(scala.util.Random.nextFloat.toString), Set.empty)
      case AnnotationValueType.DateTime =>
        (Some(OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)), None, Set.empty)
      case Select =>
        val options = annotationType.maxValueCount match {
          case Some(1) => Set(annotationType.options(0))
          case Some(2) => annotationType.options.toSet
          case _       => Set.empty[String]
        }
        (None, None, options)
    }

  def createAnnotation(): Annotation = {
    val annot = Annotation(annotationTypeId = defaultAnnotationType.id,
                           valueType      = defaultAnnotationType.valueType,
                           stringValue    = None,
                           numberValue    = None,
                           selectedValues = Set.empty[String])
    domainObjects = domainObjects + (classOf[Annotation] -> annot)
    annot
  }

  def createAnnotationWithValues(annotationType: AnnotationType): Annotation = {
    val (stringValue, numberValue, selectedValues) = createAnnotationValues(annotationType)
    val annot = createAnnotation
      .copy(stringValue = stringValue, numberValue = numberValue, selectedValues = selectedValues)
    //log.info(s"----> id: ${annotationType.id}, ${annotationType.maxValueCount}, $stringValue, $numberValue, $selectedValues")
    domainObjects = domainObjects + (classOf[Annotation] -> annot)
    annot
  }

  def createCollectionEvent(): CollectionEvent = {
    val participant         = defaultParticipant
    val collectionEventType = defaultCollectionEventType

    val id = CollectionEventId(nextIdentityAsString[CollectionEvent])
    val cevent = CollectionEvent(id = id,
                                 participantId         = participant.id,
                                 collectionEventTypeId = collectionEventType.id,
                                 version               = 0,
                                 timeAdded             = OffsetDateTime.now,
                                 timeModified          = None,
                                 slug                  = Slug(id.id),
                                 timeCompleted         = OffsetDateTime.now,
                                 visitNumber           = 1,
                                 annotations           = Set.empty)
    domainObjects = domainObjects + (classOf[CollectionEvent] -> cevent)
    cevent
  }

  def createUsableSpecimen(): UsableSpecimen = {
    val specimenDefinition = defaultCollectedSpecimenDefinition
    val location           = defaultLocation
    val inventoryId        = nextIdentityAsString[Specimen]

    val specimen = UsableSpecimen(id = SpecimenId(nextIdentityAsString[Specimen]),
                                  version              = 0,
                                  timeAdded            = OffsetDateTime.now,
                                  timeModified         = None,
                                  slug                 = Slug(inventoryId),
                                  inventoryId          = inventoryId,
                                  specimenDefinitionId = specimenDefinition.id,
                                  originLocationId     = location.id,
                                  locationId           = location.id,
                                  containerId          = None,
                                  schemaLabel          = None,
                                  timeCreated          = OffsetDateTime.now,
                                  amount               = BigDecimal(1.0))
    domainObjects = domainObjects + (classOf[UsableSpecimen] -> specimen)
    specimen
  }

  def createUnusableSpecimen(): UnusableSpecimen = {
    val specimenDefinition = defaultCollectedSpecimenDefinition
    val location           = defaultLocation
    val inventoryId        = nextIdentityAsString[Specimen]

    val specimen = UnusableSpecimen(id = SpecimenId(nextIdentityAsString[Specimen]),
                                    version              = 0,
                                    timeAdded            = OffsetDateTime.now,
                                    timeModified         = None,
                                    slug                 = Slug(inventoryId),
                                    inventoryId          = inventoryId,
                                    specimenDefinitionId = specimenDefinition.id,
                                    originLocationId     = location.id,
                                    locationId           = location.id,
                                    containerId          = None,
                                    schemaLabel          = None,
                                    timeCreated          = OffsetDateTime.now,
                                    amount               = BigDecimal(1.0))
    domainObjects = domainObjects + (classOf[UnusableSpecimen] -> specimen)
    specimen
  }

  def createDisabledCentre(): DisabledCentre = {
    val name = faker.Lorem.sentence(3)
    val centre = DisabledCentre(id = CentreId(nextIdentityAsString[Centre]),
                                version      = 0L,
                                timeAdded    = OffsetDateTime.now,
                                timeModified = None,
                                slug         = Slug(name),
                                name         = name,
                                description  = Some(nameGenerator.next[Centre]),
                                studyIds     = Set.empty,
                                locations    = Set(createLocation))

    domainObjects = domainObjects + (classOf[DisabledCentre] -> centre)
    centre
  }

  def createEnabledCentre(): EnabledCentre = {
    val name = faker.Lorem.sentence(3)
    val centre = EnabledCentre(id = CentreId(nextIdentityAsString[Centre]),
                               version      = 0L,
                               timeAdded    = OffsetDateTime.now,
                               timeModified = None,
                               slug         = Slug(name),
                               name         = name,
                               description  = Some(nameGenerator.next[Centre]),
                               studyIds     = Set.empty,
                               locations    = Set(createLocation))
    domainObjects = domainObjects + (classOf[EnabledCentre] -> centre)
    centre
  }

  def createLocation(): Location = {
    val name = faker.Lorem.sentence(3)
    val location = Location(id = LocationId(nextIdentityAsString[Location]),
                            slug           = Slug(name),
                            name           = name,
                            street         = nameGenerator.next[Location],
                            city           = nameGenerator.next[Location],
                            province       = nameGenerator.next[Location],
                            postalCode     = nameGenerator.next[Location],
                            poBoxNumber    = Some(nameGenerator.next[Location]),
                            countryIsoCode = nameGenerator.next[Location])
    domainObjects = domainObjects + (classOf[Location] -> location)
    location
  }

  def createShipment(
      originCentre:        Centre,
      originLocation:      Location,
      destinationCentre:   Centre,
      destinationLocation: Location
    ): CreatedShipment = {
    val shipment = CreatedShipment(id = ShipmentId(nextIdentityAsString[Shipment]),
                                   version               = 0L,
                                   timeAdded             = OffsetDateTime.now,
                                   timeModified          = None,
                                   courierName           = nameGenerator.next[Shipment],
                                   trackingNumber        = nameGenerator.next[Shipment],
                                   originCentreId        = originCentre.id,
                                   originLocationId      = originLocation.id,
                                   destinationCentreId   = destinationCentre.id,
                                   destinationLocationId = destinationLocation.id,
                                   timePacked            = None,
                                   timeSent              = None,
                                   timeReceived          = None,
                                   timeUnpacked          = None,
                                   timeCompleted         = None)
    domainObjects = domainObjects + (classOf[Shipment] -> shipment)
    shipment
  }

  /**
   * Assumes originCentre and destinationCentre have at least one location and use the first locations.
   */
  def createShipment(originCentre: Centre, destinationCentre: Centre): CreatedShipment =
    createShipment(originCentre,
                   originCentre.locations.head,
                   destinationCentre,
                   destinationCentre.locations.head)

  def createShipment: CreatedShipment = {
    val centre   = defaultEnabledCentre
    val location = defaultLocation
    createShipment(centre, location, centre, location)
  }

  def createPackedShipment(originCentre: Centre, destinationCentre: Centre): PackedShipment =
    createShipment(originCentre, destinationCentre).pack(OffsetDateTime.now.minusDays(10))

  def createSentShipment(originCentre: Centre, destinationCentre: Centre): SentShipment = {
    val shipment = createPackedShipment(originCentre, destinationCentre)
    shipment
      .send(shipment.timePacked.get.plusDays(1))
      .fold(err => sys.error("failed to create a sent shipment"), s => s)
  }

  def createReceivedShipment(originCentre: Centre, destinationCentre: Centre): ReceivedShipment = {
    val shipment = createSentShipment(originCentre, destinationCentre)
    shipment
      .receive(shipment.timeSent.get.plusDays(1))
      .fold(err => sys.error("failed to create a received shipment"), s => s)
  }

  def createUnpackedShipment(originCentre: Centre, destinationCentre: Centre): UnpackedShipment = {
    val shipment = createReceivedShipment(originCentre, destinationCentre)
    shipment
      .unpack(shipment.timeReceived.get.plusDays(1))
      .fold(err => sys.error("failed to create a unpacked shipment"), s => s)
  }

  def createCompletedShipment(originCentre: Centre, destinationCentre: Centre): CompletedShipment = {
    val shipment = createUnpackedShipment(originCentre, destinationCentre)
    shipment
      .complete(shipment.timeReceived.get.plusDays(1))
      .fold(err => sys.error("failed to create a completed shipment"), s => s)
  }

  def createLostShipment(originCentre: Centre, destinationCentre: Centre): LostShipment =
    createSentShipment(originCentre, destinationCentre).lost

  def createShipmentSpecimen(): ShipmentSpecimen = {
    val specimen = defaultUsableSpecimen
    val shipment = defaultShipment

    val shipmentSpecimen = ShipmentSpecimen(id = ShipmentSpecimenId(nextIdentityAsString[ShipmentSpecimen]),
                                            version             = 0L,
                                            timeAdded           = OffsetDateTime.now,
                                            timeModified        = None,
                                            shipmentId          = shipment.id,
                                            specimenId          = specimen.id,
                                            state               = ShipmentItemState.Present,
                                            shipmentContainerId = None)
    domainObjects = domainObjects + (classOf[ShipmentSpecimen] -> shipmentSpecimen)
    shipmentSpecimen
  }

  def createShipmentContainer(): ShipmentContainer =
    ???
  // val container = defaultContainer
  // val shipment = defaultShipment

  // val shipmentContainer = ShipmentContainer(
  //     id                  = ShipmentContainerId(nextIdentityAsString[ShipmentContainer]),
  //     version             = 0L,
  //     timeAdded           = OffsetDateTime.now,
  //     timeModified        = None,
  //     shipmentId          = shipment.id,
  //     containerId         = container.id,
  //     state               = ShipmentItemState.Present)
  // domainObjects = domainObjects + (classOf[ShipmentContainer] -> shipmentContainer)
  // shipmentContainer

  def createStorageContainerType(centre: Centre, schema: ContainerSchema): StorageContainerType = {
    val name = nameGenerator.next[ContainerType]
    val containerType = StorageContainerType(id = ContainerTypeId(nextIdentityAsString[ContainerType]),
                                             version      = 0L,
                                             timeAdded    = OffsetDateTime.now,
                                             timeModified = None,
                                             slug         = Slug(name),
                                             name         = name,
                                             description  = Some(nameGenerator.next[ContainerType]),
                                             centreId     = centre.id,
                                             schemaId     = schema.id,
                                             shared       = false,
                                             enabled      = false)
    domainObjects = domainObjects + (classOf[StorageContainerType] -> containerType)
    containerType
  }

  def createStorageContainerType(): StorageContainerType =
    createStorageContainerType(defaultEnabledCentre, defaultContainerSchema)

  def createSpecimenContainerType(centre: Centre, schema: ContainerSchema): SpecimenContainerType = {
    val name = nameGenerator.next[ContainerType]
    val containerType = SpecimenContainerType(id = ContainerTypeId(nextIdentityAsString[ContainerType]),
                                              version      = 0L,
                                              timeAdded    = OffsetDateTime.now,
                                              timeModified = None,
                                              slug         = Slug(name),
                                              name         = name,
                                              description  = Some(nameGenerator.next[ContainerType]),
                                              centreId     = centre.id,
                                              schemaId     = schema.id,
                                              shared       = true,
                                              enabled      = false)
    domainObjects = domainObjects + (classOf[SpecimenContainerType] -> containerType)
    containerType
  }

  def createSpecimenContainerType(): SpecimenContainerType =
    createSpecimenContainerType(defaultEnabledCentre, defaultContainerSchema)

  def createContainerSchema(labels: Set[String]): ContainerSchema = {
    val name = faker.Lorem.sentence(3)
    val id   = ContainerSchemaId(nextIdentityAsString[ContainerSchema])
    val containerSchema = ContainerSchema(version = 0L,
                                          timeAdded    = OffsetDateTime.now,
                                          timeModified = None,
                                          id           = id,
                                          slug         = Slug(name),
                                          name         = name,
                                          description  = Some(nameGenerator.next[ContainerSchema]),
                                          shared       = false,
                                          centreId     = defaultEnabledCentre.id,
                                          labels       = labels)
    domainObjects = domainObjects + (classOf[ContainerSchema] -> containerSchema)
    containerSchema
  }

  def createContainerSchema(): ContainerSchema =
    createContainerSchema(Set(nameGenerator.next[ContainerSchemaLabel]))

  def createContainerSchemaLabel(): ContainerSchemaLabel =
    ContainerSchemaLabel(schemaId = defaultContainerSchema.id,
                         label    = nextIdentityAsString[ContainerSchemaLabel])

  def createContainerConstraints(): ContainerConstraints = {
    val name = nameGenerator.next[ContainerConstraints]
    val containerConstraints =
      ContainerConstraints(name              = name,
                           description       = Some(nameGenerator.next[ContainerConstraints]),
                           anatomicalSources = Set(AnatomicalSourceType.Blood),
                           preservationTypes = Set(PreservationType.FrozenSpecimen),
                           specimenTypes     = Set(SpecimenType.BuffyCoat))
    domainObjects = domainObjects + (classOf[ContainerConstraints] -> containerConstraints)
    containerConstraints
  }

  def createRootContainer(centre: Centre, containerType: StorageContainerType): RootContainer = {
    val inventoryId = nameGenerator.next[Container]
    val locationId  = centre.locations.headOption.value.id
    val container = RootContainer(id = ContainerId(nextIdentityAsString[Container]),
                                  version         = 0L,
                                  timeAdded       = OffsetDateTime.now,
                                  timeModified    = None,
                                  slug            = Slug(inventoryId),
                                  inventoryId     = inventoryId,
                                  label           = nameGenerator.next[Container],
                                  enabled         = false,
                                  containerTypeId = containerType.id,
                                  centreId        = centre.id,
                                  locationId      = locationId,
                                  temperature     = PreservationTemperature.Minus80celcius,
                                  constraints     = None)
    domainObjects = domainObjects + (classOf[RootContainer] -> container)
    container
  }

  def createRootContainer(): RootContainer =
    createRootContainer(defaultEnabledCentre, defaultStorageContainerType)

  def createStorageContainer(
      containerType: StorageContainerType,
      parent:        Container,
      schemaLabel:   ContainerSchemaLabel
    ): StorageContainer = {
    val inventoryId = nameGenerator.next[Container]
    val container = StorageContainer(id = ContainerId(nextIdentityAsString[Container]),
                                     version         = 0L,
                                     timeAdded       = OffsetDateTime.now,
                                     timeModified    = None,
                                     slug            = Slug(inventoryId),
                                     inventoryId     = inventoryId,
                                     enabled         = false,
                                     containerTypeId = containerType.id,
                                     parentId        = parent.id,
                                     schemaLabel     = schemaLabel,
                                     constraints     = None)
    domainObjects = domainObjects + (classOf[StorageContainer] -> container)
    container
  }

  def createStorageContainer(): StorageContainer = {
    val schemaLabel =
      ContainerSchemaLabel(defaultContainerSchema.id, defaultContainerSchema.labels.headOption.value)
    createStorageContainer(defaultStorageContainerType, defaultRootContainer, schemaLabel)
  }

  def createSpecimenContainer(
      containerType: SpecimenContainerType,
      parent:        StorageContainer,
      schemaLabel:   ContainerSchemaLabel
    ): SpecimenContainer = {
    val inventoryId = nameGenerator.next[Container]
    val container = SpecimenContainer(id = ContainerId(nextIdentityAsString[Container]),
                                      version         = 0L,
                                      timeAdded       = OffsetDateTime.now,
                                      timeModified    = None,
                                      slug            = Slug(inventoryId),
                                      inventoryId     = inventoryId,
                                      containerTypeId = containerType.id,
                                      parentId        = parent.id,
                                      schemaLabel     = schemaLabel)
    domainObjects = domainObjects + (classOf[SpecimenContainer] -> container)
    container
  }

  def createSpecimenContainer(): SpecimenContainer = {
    val schemaLabel =
      ContainerSchemaLabel(defaultContainerSchema.id, defaultContainerSchema.labels.headOption.value)
    createSpecimenContainer(defaultSpecimenContainerType, defaultStorageContainer, schemaLabel)
  }

  // def defaultRegisteredUser: RegisteredUser = {
  //   defaultObject(classOf[RegisteredUser], createRegisteredUser)
  // }

  def defaultActiveUser: ActiveUser =
    defaultObject(classOf[ActiveUser], createActiveUser)

  def defaultLockedUser: LockedUser =
    defaultObject(classOf[LockedUser], createLockedUser)

  def defaultDisabledStudy: DisabledStudy =
    defaultObject(classOf[DisabledStudy], createDisabledStudy)

  def defaultEnabledStudy: EnabledStudy =
    defaultObject(classOf[EnabledStudy], createEnabledStudy)

  def defaultCollectionEventType: CollectionEventType =
    defaultObject(classOf[CollectionEventType], createCollectionEventType)

  def defaultCollectedSpecimenDefinition: CollectedSpecimenDefinition =
    defaultObject(classOf[CollectedSpecimenDefinition], createCollectedSpecimenDefinition)

  def defaultProcessedSpecimenDefinition: ProcessedSpecimenDefinition =
    defaultObject(classOf[ProcessedSpecimenDefinition], createProcessedSpecimenDefinition)

  def defaultAnnotationType: AnnotationType =
    defaultObject(classOf[AnnotationType], createAnnotationType)

  def defaultProcessingType: ProcessingType =
    defaultObject(classOf[ProcessingType], createProcessingType)

  def defaultParticipant: Participant =
    defaultObject(classOf[Participant], createParticipant)

  def defaultAnnotation: Annotation =
    defaultObject(classOf[Annotation], createAnnotation)

  def defaultCollectionEvent(): CollectionEvent =
    defaultObject(classOf[CollectionEvent], createCollectionEvent)

  def defaultUsableSpecimen: UsableSpecimen =
    defaultObject(classOf[UsableSpecimen], createUsableSpecimen)

  def defaultUnusableSpecimen: UnusableSpecimen =
    defaultObject(classOf[UnusableSpecimen], createUnusableSpecimen)

  def defaultDisabledCentre: DisabledCentre =
    defaultObject(classOf[DisabledCentre], createDisabledCentre)

  def defaultEnabledCentre: EnabledCentre =
    defaultObject(classOf[EnabledCentre], createEnabledCentre)

  def defaultLocation: Location =
    defaultObject(classOf[Location], createLocation)

  def defaultContainerSchema: ContainerSchema =
    defaultObject(classOf[ContainerSchema], createContainerSchema)

  def defaultShipment: Shipment =
    defaultObject(classOf[Shipment], createShipment)

  def defaultShipmentSpecimen: ShipmentSpecimen =
    defaultObject(classOf[ShipmentSpecimen], createShipmentSpecimen)

  def defaultShipmentContainer: ShipmentContainer =
    defaultObject(classOf[ShipmentContainer], createShipmentContainer)

  def defaultStorageContainerType: StorageContainerType =
    defaultObject(classOf[StorageContainerType], createStorageContainerType)

  def defaultSpecimenContainerType: SpecimenContainerType =
    defaultObject(classOf[SpecimenContainerType], createSpecimenContainerType)

  def defaultRootContainer: RootContainer =
    defaultObject(classOf[RootContainer], createRootContainer)

  def defaultStorageContainer: StorageContainer =
    defaultObject(classOf[StorageContainer], createStorageContainer)

  def defaultSpecimenContainer: SpecimenContainer =
    defaultObject(classOf[SpecimenContainer], createSpecimenContainer)

  /** Retrieves the class from the map, or calls 'create' if value does not exist
   */
  private def defaultObject[T](key: Class[T], create: => T): T =
    domainObjects.get(key).fold { create } { obj =>
      key.cast(obj)
    }
}
