/**
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2017 Canadian BioSample Repository (CBSR)
 */

/* @ngInject */
function PermissionNameFactory($q,
                         $log,
                         biobankApi,
                         EntityName,
                         DomainEntity,
                         DomainError) {

  /**
   * @classdesc A PermissionName contains the ID, and name for a permission.
   *
   * Please do not use this constructor. It is meant for internal use.
   *
   * Use AccessItemNameFactory to create objects of this type from a server response.
   *
   * @class
   * @memberOf domain.access
   * @extends domain.DomainEntity
   *
   * @param {object} [obj={}] - An initialization object whose properties are the same as the members from
   * this class. Objects of this type are usually returned by the server's REST API.
   */
  class PermissionName extends EntityName {

    constructor(obj = {}) {
      super(obj)
    }
  }

  /**
   * Creates a PermissionName, but first it validates <code>obj</code> to ensure that it has a valid schema.
   *
   * @param {object} [obj={}] - An initialization object whose properties are the same as the members from
   * this class. Objects of this type are usually returned by the server's REST API.
   *
   * @returns {domain.access.Permission} A permission created from the given object.
   *
   * @see {@link domain.access.PermissionName.asyncCreate|asyncCreate()} when you need to create
   * a permission within asynchronous code.
   */
  PermissionName.create = function (obj) {
    var validation = EntityName.isValid(obj);
    if (!validation.valid) {
      $log.error(validation.message);
      throw new DomainError(validation.message);
    }
    return new PermissionName(obj);
  };

  return PermissionName;
}

export default ngModule => ngModule.factory('PermissionName', PermissionNameFactory)