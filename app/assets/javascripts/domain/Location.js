/**
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2015 Canadian BioSample Repository (CBSR)
 */

/**
 *
 */
/* @ngInject */
function LocationFactory($log, DomainEntity, DomainError) {

  /**
   * Location is a value object.
   */
  function Location(obj) {
    this.id             = null;
    this.name           = '';
    this.street         = '';
    this.city           = '';
    this.province       = '';
    this.postalCode     = '';
    this.poBoxNumber    = null;
    this.countryIsoCode = '';

    DomainEntity.call(this, Location.SCHEMA, obj);
  }

  Location.prototype = Object.create(DomainEntity.prototype);
  Location.prototype.constructor = Location;

  Location.SCHEMA = {
    'id': 'Location',
    'type': 'object',
    'properties': {
      'id':             { 'type': 'string'},
      'slug':           { 'type': 'string'},
      'name':           { 'type': 'string'},
      'street':         { 'type': 'string'},
      'city':           { 'type': 'string'},
      'province':       { 'type': 'string'},
      'postalCode':     { 'type': 'string'},
      'poBoxNumber':    { 'type': 'string'},
      'countryIsoCode': { 'type': 'string'}
    },
    'required': [
      'id',
      'slug',
      'name',
      'street',
      'city',
      'province',
      'postalCode',
      'countryIsoCode',
    ]
  };

  Location.isValid = function(obj) {
    return DomainEntity.isValid(Location.SCHEMA, null, obj);
  };

  /**
   * Validates the object before creating it, ensuring that it contains the required fields.
   *
   * Should be used to create a location from the object returned by the server.
   */
  Location.create = function(obj) {
    var validation = Location.isValid(obj);

    if (!validation.valid) {
      $log.error('invalid object from server: ' + validation.message);
      throw new DomainError('invalid object from server: ' + validation.message);
    }
    return new Location(obj);
  };

  return Location;
}

export default ngModule => ngModule.factory('Location', LocationFactory)
