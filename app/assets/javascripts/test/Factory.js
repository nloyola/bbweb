/**
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2015 Canadian BioSample Repository (CBSR)
 */
import _ from 'lodash';
import faker  from 'faker';
import moment from 'moment';

/*
 * Generates JSON domain entities as if returned by the server.
 *
 * This has to be an AngularJS service so that it's dependencies from the real application
 * can be injected (i.e. AnnotationValueType).
 */
export default class Factory {

  constructor(AnnotationValueType,
              AnnotationMaxValueCount,
              AnatomicalSourceType,
              PreservationTemperatureType,
              PreservationType,
              SpecimenType,
              StudyState,
              CentreState,
              UserState,
              SpecimenState,
              ShipmentState,
              ShipmentItemState) {

    this.AnnotationValueType = AnnotationValueType;
    this.AnnotationMaxValueCount = AnnotationMaxValueCount;
    this.AnatomicalSourceType = AnatomicalSourceType;
    this.PreservationTemperatureType = PreservationTemperatureType;
    this.PreservationType = PreservationType;
    this.SpecimenType = SpecimenType;
    this.StudyState = StudyState;
    this.CentreState = CentreState;
    this.UserState = UserState;
    this.SpecimenState = SpecimenState;
    this.ShipmentState = ShipmentState;
    this.ShipmentItemState = ShipmentItemState;

    this.defaultEntities = {};
    this.entityCount = 0;
    this.valueTypeCount = 0;
    this.commonFieldNames = _.keys(this.commonFields());
  }

  ENTITY_NAME_PROCESSING_TYPE()       { return 'processingType'; }
  ENTITY_NAME_SPECIMEN_LINK_TYPE()    { return 'specimenLinkType'; }
  ENTITY_NAME_COLLECTION_EVENT_TYPE() { return 'collectionEventType'; }
  ENTITY_NAME_SPECIMEN_GROUP()        { return 'specimenGroup'; }
  ENTITY_NAME_ANNOTATION_TYPE()       { return 'annotationType'; }
  ENTITY_NAME_STUDY()                 { return 'study'; }
  ENTITY_NAME_ANNOTATION()            { return 'annotation'; }

  ENTITY_NAME_PARTICIPANT()           { return 'participant'; }
  ENTITY_NAME_COLLECTION_EVENT()      { return 'collectionEvent'; }
  ENTITY_NAME_SPECIMEN()              { return 'specimen'; }

  ENTITY_NAME_CENTRE()                { return 'centre'; }
  ENTITY_NAME_LOCATION()              { return 'location'; }
  ENTITY_NAME_SHIPMENT()              { return 'shipment'; }
  ENTITY_NAME_SHIPMENT_SPECIMEN()     { return 'shipmentSpecimen'; }

  ENTITY_NAME_USER()                  { return 'user'; }

  ENTITY_NAME_MEMBERSHIP_BASE()       { return 'membershipBase'; }
  ENTITY_NAME_MEMBERSHIP()            { return 'membership'; }

  commonFields() {
    return {
      version:      0,
      timeAdded:    moment(faker.date.recent(10)).format(),
      timeModified: moment(faker.date.recent(5)).format()
    };
  }

  stringNext() {
    return this.domainEntityNameNext();
  }

  emailNext() {
    return faker.internet.email();
  }

  urlNext() {
    return faker.internet.url();
  }

  updateDefaultEntity(entityName, entity) {
    this.defaultEntities[entityName] = entity;
    this.entityCount += 1;
  }

  defaultEntity(entityName, createFunc) {
    if (_.isUndefined(this.defaultEntities[entityName])) {
      createFunc.call(this);
    }
    return this.defaultEntities[entityName];
  }

  /**
   * Generates a unique name for a domain entity type. If domain entity type is undefined, then a unique
   * string is generated.
   *
   * @param domainEntityType the name of the domain entity type. Eg: 'study', 'centre', 'user', etc.
   */
  domainEntityNameNext(domainEntityType) {
    domainEntityType = domainEntityType || 'string';
    return _.uniqueId(domainEntityType + '_');
  }

  specimenLinkType(options) {
    const processingType = this.defaultProcessingType(),
        defaults = {
          id:                    this.domainEntityNameNext(this.ENTITY_NAME_SPECIMEN_LINK_TYPE()),
          processingTypeId:      processingType.id,
          expectedInputChange:   faker.random.number({precision: 0.5}),
          expectedOutputChange:  faker.random.number({precision: 0.5}),
          inputGroupId:          null,
          outputGroupId:         null,
          inputCount:            faker.random.number(5) + 1,
          outputCount:           faker.random.number(5) + 1,
          inputContainerTypeId:  null,
          outputContainerTypeId: null
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        slt = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));

    this.updateDefaultEntity(this.ENTITY_NAME_SPECIMEN_LINK_TYPE(), slt);
    return slt;
  }

  defaultSpecimenLinkType() {
    return this.defaultEntity(this.ENTITY_NAME_SPECIMEN_LINK_TYPE(), this.specimenLinkType);
  }

  processingType(options) {
    var study = this.defaultStudy(),
        defaults = {
          id:          this.domainEntityNameNext(this.ENTITY_NAME_PROCESSING_TYPE()),
          studyId:     study.id,
          name:        this.stringNext(),
          description: faker.lorem.sentences(4),
          enabled:     false
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        pt = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_PROCESSING_TYPE(), pt);
    return pt;
  }

  defaultProcessingType() {
    return this.defaultEntity(this.ENTITY_NAME_PROCESSING_TYPE(), this.processingType);
  }

  /**
   * Returns a collection event type as returned by the server.
   */
  collectionEventType(options) {
    var study = this.defaultStudy(),
        defaults = {
          id:                   this.domainEntityNameNext(this.ENTITY_NAME_COLLECTION_EVENT_TYPE()),
          studyId:              study.id,
          name:                 this.stringNext(),
          description:          faker.lorem.sentences(4),
          specimenDescriptions: [],
          annotationTypes:      [],
          recurring:            false
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        cet = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_COLLECTION_EVENT_TYPE(), cet);
    return cet;
  }

  defaultCollectionEventType() {
    return this.defaultEntity(this.ENTITY_NAME_COLLECTION_EVENT_TYPE(), this.collectionEventType);
  }

  randomAnatomicalSourceType() {
    return faker.random.arrayElement(_.values(this.AnatomicalSourceType));
  }

  randomPreservationType() {
    return faker.random.arrayElement(_.values(this.PreservationType));
  }

  randomPreservationTemperatureTypeType() {
    return faker.random.arrayElement(_.values(this.PreservationTemperatureType));
  }

  randomSpecimenType() {
    return faker.random.arrayElement(_.values(this.SpecimenType));
  }

  specimenGroup(options) {
    var study = this.defaultStudy(),
        defaults = {
          id:                          this.domainEntityNameNext(this.ENTITY_NAME_SPECIMEN_GROUP()),
          studyId:                     study.id,
          name:                        this.stringNext(),
          description:                 faker.lorem.sentences(4),
          units:                       'mL',
          anatomicalSourceType:        this.randomAnatomicalSourceType(),
          preservationType:            this.randomPreservationType(),
          preservationTemperatureType: this.randomPreservationTemperatureTypeType(),
          specimenType:                this.randomSpecimenType()
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        sg = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_SPECIMEN_GROUP(), sg);
    return sg;
  }

  defaultSpecimenGroup() {
    return this.defaultEntity(this.ENTITY_NAME_SPECIMEN_GROUP(), this.specimenGroup);
  }

  study(options) {
    var defaults =  { id:              this.domainEntityNameNext(this.ENTITY_NAME_STUDY()),
                      name:            this.stringNext(),
                      description:     faker.lorem.sentences(4),
                      annotationTypes: [],
                      state:           this.StudyState.DISABLED
                    },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        s = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_STUDY(), s);
    return s;
  }

  defaultStudy() {
    return this.defaultEntity(this.ENTITY_NAME_STUDY(), this.study);
  }

  entityNameDto(createFunc, options) {
    var c;
    options = options || {};
    c = createFunc.call(this, _.pick(options, ['id', 'name', 'state']));
    return _.pick(c, ['id', 'name', 'state']);
  }

  studyNameDto(options) {
    return this.entityNameDto(this.study, options);
  }

  centreNameDto(options) {
    return this.entityNameDto(this.centre, options);
  }

  userNameDto(options) {
    return this.entityNameDto(this.user, options);
  }

  /**
   * If this.defaultStudy has annotation types, then participant will have annotations based on the study's,
   * unless options.annotationTypes is defined.
   */
  participant(options) {
    var study = this.defaultStudy(),
        defaults = {
          id:          this.domainEntityNameNext(this.ENTITY_NAME_PARTICIPANT()),
          studyId:     study.id,
          uniqueId:    this.domainEntityNameNext(this.ENTITY_NAME_PARTICIPANT()),
          annotations: []
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        p;

    options = options || {};

    p = _.extend(defaults, this.commonFields(), _.pick(options, validKeys));

    if (!options.annotations) {
      // assign annotation types
      if (options.annotationTypes) {
        p.annotations = this.annotations(options.annotationTypes);
      } else if (study.annotationTypes) {
        p.annotations = this.annotations(study.annotationTypes);
      }
    }

    this.updateDefaultEntity(this.ENTITY_NAME_PARTICIPANT(), p);
    return p;
  }

  defaultParticipant() {
    return this.defaultEntity(this.ENTITY_NAME_PARTICIPANT(), this.participant);
  }

  collectionEvent(options) {
    var participant = this.defaultParticipant(),
        collectionEventType = this.defaultCollectionEventType(),
        defaults = {
          id:                    this.domainEntityNameNext(this.ENTITY_NAME_COLLECTION_EVENT()),
          participantId:         participant.id,
          collectionEventType:   collectionEventType,
          collectionEventTypeId: collectionEventType.id,
          timeCompleted:         moment(faker.date.recent(10)).format(),
          visitNumber:           1,
          annotations:           []
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        ce;

    options = options || {};
    ce = _.extend(defaults, this.commonFields(), _.pick(options, validKeys));

    if (!options.annotations) {
      // assign annotation types
      if (options.annotationTypes) {
        ce.annotations = this.annotations(options.annotationTypes);
      } else if (collectionEventType.annotationTypes) {
        ce.annotations = this.annotations(collectionEventType.annotationTypes);
      }
    }

    this.updateDefaultEntity(this.ENTITY_NAME_COLLECTION_EVENT(), ce);
    return ce;
  }

  defaultCollectionEvent() {
    return this.defaultEntity(this.ENTITY_NAME_COLLECTION_EVENT(), this.collectionEvent);
  }

  centreLocationInfo(centre) {
    if (!centre.locations || (centre.locations.length < 1)) {
      throw new Error('centre does not have any locations');
    }
    return {
      centreId:   centre.id,
      locationId: centre.locations[0].id,
      name:       centre.name +': ' + centre.locations[0].name
    };
  }

  centreLocationDto(centre) {
    if (!centre.locations || (centre.locations.length < 1)) {
      throw new Error('centre does not have any locations');
    }
    return {
      centreId:     centre.id,
      locationId:   centre.locations[0].id,
      centreName:   centre.name,
      locationName: centre.locations[0].name
    };
  }

  specimen(options) {
    var ceventType = this.collectionEventType({ specimenDescriptions: [ this.collectionSpecimenDescription() ] }),
        ctr = this.centre({ locations: [ this.location() ]}),
        defaults = {
          id:                    this.domainEntityNameNext(this.ENTITY_NAME_SPECIMEN()),
          inventoryId:           this.domainEntityNameNext(this.ENTITY_NAME_SPECIMEN()),
          specimenDescriptionId: null,
          originLocationInfo:    null,
          locationInfo:          null,
          timeCreated:           moment(faker.date.recent(10)).format(),
          amount:                1,
          state:                 this.SpecimenState.USABLE
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        spc;

    options = options || {};

    if (ceventType.specimenDescriptions && (ceventType.specimenDescriptions.length > 0)) {
      defaults.specimenDescriptionId = ceventType.specimenDescriptions[0].id;
    }

    if (ctr.locations && (ctr.locations.length > 0)) {
      defaults.originLocationInfo = this.centreLocationInfo(ctr);
      defaults.locationInfo = defaults.originLocationInfo;
    }

    spc = _.extend(defaults, this.commonFields(), _.pick(options, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_SPECIMEN(), spc);
    return spc;
  }

  defaultSpecimen() {
    return this.defaultEntity(this.ENTITY_NAME_SPECIMEN(), this.specimen);
  }

  centre(options) {
    var defaults = { id:          this.domainEntityNameNext(this.ENTITY_NAME_CENTRE()),
                     name:        this.stringNext(),
                     description: this.stringNext(),
                     state:       this.CentreState.DISABLED,
                     studyNames:  [],
                     locations:   []
                   },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        c = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_CENTRE(), c);
    return c;
  }

  defaultCentre() {
    return this.defaultEntity(this.ENTITY_NAME_CENTRE(), this.centre);
  }

  shipment(options) {
    var loc = this.location(),
        ctr = this.centre({ locations: [ loc ]}),
        locationInfo = {
          centreId: ctr.id,
          locationId: loc.id,
          name: ctr.name + ': ' + loc.name
        },
        defaults = {
          id:               this.domainEntityNameNext(this.ENTITY_NAME_SHIPMENT()),
          state:            this.ShipmentState.CREATED,
          courierName:      this.stringNext(),
          trackingNumber:   this.stringNext(),
          fromLocationInfo: locationInfo,
          toLocationInfo:   locationInfo,
          specimenCount:    0
        },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        s = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_SHIPMENT(), s);
    return s;
  }

  defaultShipment() {
    return this.defaultEntity(this.ENTITY_NAME_SHIPMENT(), this.shipment);
  }

  shipmentSpecimen(options) {
    var shipment = this.defaultShipment(),
        specimen = this.defaultSpecimen(),
        defaults = { id:           this.domainEntityNameNext(this.ENTITY_NAME_SHIPMENT()),
                     state:        this.ShipmentItemState.PRESENT,
                     shipmentId:   shipment.id,
                     specimen:     specimen
                   },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        ss = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_SHIPMENT_SPECIMEN(), ss);
    return ss;
  }

  defaultShipmentSpecimen() {
    return this.defaultEntity(this.ENTITY_NAME_SHIPMENT_SPECIMEN(), this.shipment);
  }

  user(options) {
    var defaults = { id:         this.domainEntityNameNext(this.ENTITY_NAME_USER()),
                     name:       this.stringNext(),
                     email:      this.stringNext(),
                     avatarUrl:  null,
                     state:      this.UserState.REGISTERED,
                     roles:      []
                   },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        membership,
        u;

    options = options || {};
    if (_.isNil(options.membership)) {
      options.membership = {};
    }
    membership = this.userMembership(options.membership);
    u = _.extend(defaults, this.commonFields(), _.pick(options || {}, validKeys), { membership: membership });
    this.updateDefaultEntity(this.ENTITY_NAME_USER(), u);
    return u;
  }

  defaultUser() {
    return this.defaultEntity(this.ENTITY_NAME_USER(), this.user);
  }

  /**
   * @param {ValueType} option.valueType the type of annotation Type to create. Valid types are: Text,
   * Number, DateTime and Select.
   *
   * @param {Int} option.maxValueCount when valueType is 'Select', use 1 for single selection or '2' for
   * multiple selection.
   */
  annotationType(options) {
    var defaults = { id:            this.domainEntityNameNext(this.ENTITY_NAME_ANNOTATION_TYPE()),
                     name:          this.stringNext(),
                     description:   null,
                     valueType:     this.AnnotationValueType.TEXT,
                     options:       [],
                     maxValueCount: this.AnnotationMaxValueCount.NONE,
                     required:      false
                   },
        validKeys = _.keys(defaults),
        at;

    options = options || {};

    if (!options.valueType) {
      options.valueType = this.AnnotationValueType.TEXT;
    }

    if (options.valueType === this.AnnotationValueType.SELECT) {
      if (_.isUndefined(options.maxValueCount)) {
        options.maxValueCount = this.AnnotationMaxValueCount.SELECT_SINGLE;
      }

      if (_.isUndefined(options.options)) {
        options.options = _.range(2).map(() => this.domainEntityNameNext(this.ENTITY_NAME_ANNOTATION_TYPE()));
      }
    }

    at = _.extend(defaults, _.pick(options, validKeys));
    this.valueTypeCount += 1;
    return at;
  }

  allAnnotationTypes() {
    var annotationTypes = _.values(this.AnnotationValueType)
        .map((valueType) => this.annotationType({ valueType: valueType }));
    annotationTypes.push(this.annotationType({
      valueType:     this.AnnotationValueType.SELECT,
      maxValueCount: this.AnnotationMaxValueCount.SELECT_MULTIPLE,
      options:       [ 'opt1', 'opt2', 'opt3' ]
    }));
    return annotationTypes;
  }

  collectionSpecimenDescription(options) {
    var defaults = { id:                          this.domainEntityNameNext(this.ENTITY_NAME_SPECIMEN_GROUP()),
                     name:                        this.stringNext(),
                     description:                 faker.lorem.sentences(4),
                     units:                       'mL',
                     anatomicalSourceType:        this.randomAnatomicalSourceType(),
                     preservationType:            this.randomPreservationType(),
                     preservationTemperatureType: this.randomPreservationTemperatureTypeType(),
                     specimenType:                this.randomSpecimenType(),
                     maxCount:                    1,
                     amount:                      0.5
                   },
        validKeys = _.keys(defaults),
        spec = _.extend(defaults, _.pick(options || {}, validKeys));
    this.valueTypeCount += 1;
    return spec;
  }

  /**
   * @param options.value The value for the annotation.
   */
  annotation(options, annotationType) {
    var defaults = { annotationTypeId: null,
                     stringValue:      null,
                     numberValue:      null,
                     selectedValues:   []
                   },
        validKeys = _.keys(defaults),
        annotation = _.extend(defaults, _.pick(options || {}, validKeys));

    options = options || {};
    annotationType = annotationType || {};

    if (annotationType.id) {
      annotation.annotationTypeId = annotationType.id;
    }

    if (!_.isUndefined(options.value) && annotationType.valueType) {
      switch (annotationType.valueType) {
      case this.AnnotationValueType.TEXT:
      case this.AnnotationValueType.DATE_TIME:
        annotation.stringValue = options.value;
        break;

      case this.AnnotationValueType.NUMBER:
        annotation.numberValue = options.value;
        break;

      case this.AnnotationValueType.SELECT:
        if (options.value !== '') {
          if (annotationType.maxValueCount === this.AnnotationMaxValueCount.SELECT_SINGLE) {
            annotation.selectedValues =  [ options.value ];
          } else if (annotationType.maxValueCount === this.AnnotationMaxValueCount.SELECT_MULTIPLE) {
            annotation.selectedValues = options.value;
          } else {
            throw new Error('invalid max value count for annotation: ' + annotationType.maxValueCount);
          }
        }
        break;

      default:
        throw new Error('invalid annotation value type: ' + annotationType.valueType);
      }
    }

    return annotation;
  }

  valueForAnnotation(annotationType) {
    switch (annotationType.valueType) {

    case this.AnnotationValueType.TEXT:
      return this.stringNext();

    case this.AnnotationValueType.NUMBER:
      return faker.random.number({precision: 0.05}).toString();

    case this.AnnotationValueType.DATE_TIME:
      // has to be in UTC format, with no seconds or milliseconds
      return moment(faker.date.past(1))
        .set({
          'millisecond': 0,
          'second':      0
        })
        .utc()
        .format();

    case this.AnnotationValueType.SELECT:
      if (annotationType.maxValueCount === this.AnnotationMaxValueCount.SELECT_SINGLE) {
        return annotationType.options[0];
      } else if (annotationType.maxValueCount === this.AnnotationMaxValueCount.SELECT_MULTIPLE) {
        return annotationType.options;
      } else {
        throw new Error('invalid max value count: ' + annotationType.maxValueCount);
      }
    }

    throw new Error('invalid value type: ' + annotationType.valueType);
  }

  annotations(annotationTypes) {
    return annotationTypes.map((annotationType) => {
      var value = this.valueForAnnotation(annotationType);
      return this.annotation({ value: value }, annotationType);
    });
  }

  /**
   * This is a value object, so it does not have the common fields.
   */
  location(options) {
    var defaults = { id:             this.domainEntityNameNext(this.ENTITY_NAME_LOCATION()),
                     name:           this.stringNext(),
                     street:         faker.address.streetAddress(),
                     city:           faker.address.city(),
                     province:       faker.address.state(),
                     postalCode:     faker.address.zipCode(),
                     poBoxNumber:    faker.address.zipCode(),
                     countryIsoCode: faker.address.country()
                   },
        validKeys = _.keys(defaults),
        at = _.extend(defaults, _.pick(options || {}, validKeys));
    this.valueTypeCount += 1;
    return at;
  }

  centreLocations(centres) {
    return _.flatMap(centres, function (centre) {
      return centre.locations.map((location) => ({
        centreId:     centre.id,
        locationId:   location.id,
        centreName:   centre.name,
        locationName: location.name
      }));
    });
  }

  pagedResult(entities, options) {
    options = options || {};

    return {
      items:    entities,
      page:     1,
      offset:   0,
      total:    entities.length,
      limit:    5,
      next:     2,
      maxPages: options.maxPages || 4
    };
  }

  membershipEntityData() {
    return {
      id:   this.stringNext(),
      name: this.stringNext()
    };
  }

  membershipEntitySet() {
    return { allEntities: false, entityData: [] };
  }

  membershipBase(options) {
    var defaults =  { id:           this.domainEntityNameNext(this.ENTITY_NAME_MEMBERSHIP_BASE()),
                      name:         this.stringNext(),
                      description:  faker.lorem.sentences(4),
                      studyData:    this.membershipEntitySet(),
                      centreData:   this.membershipEntitySet()
                    },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        m = _.extend({}, defaults, this.commonFields(), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_MEMBERSHIP_BASE(), m);
    return m;
  }

  defaultMembershipBase() {
    return this.defaultEntity(this.ENTITY_NAME_MEMBERSHIP_BASE(), this.membershipBase);
  }

  membership(options) {
    var defaults =  { userData: [] },
        validKeys = this.commonFieldNames.concat(_.keys(defaults)),
        m = _.extend(defaults, this.membershipBase(options), _.pick(options || {}, validKeys));
    this.updateDefaultEntity(this.ENTITY_NAME_MEMBERSHIP(), m);
    return m;
  }

  defaultMembership() {
    return this.defaultEntity(this.ENTITY_NAME_MEMBERSHIP(), this.membership);
  }

  userMembership(options) {
    return this.membershipBase(options);
  }

}
