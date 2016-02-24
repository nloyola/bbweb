/**
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2015 Canadian BioSample Repository (CBSR)
 */
define(['angular', 'underscore', 'tv4'], function(angular, _, tv4) {
  'use strict';

  AnnotationTypeFactory.$inject = [
    'funutils',
    'validationService',
    'ConcurrencySafeEntity',
    'AnnotationValueType',
    'AnnotationMaxValueCount'
  ];

  /**
   *
   */
  function AnnotationTypeFactory(funutils,
                                 validationService,
                                 ConcurrencySafeEntity,
                                 AnnotationValueType,
                                 AnnotationMaxValueCount) {

    var schema = {
      'id': 'AnnotationType',
      'type': 'object',
      'properties': {
        'uniqueId':        { 'type': 'string'  },
        'name':            { 'type': 'string'  },
        'description':     { 'type': 'string'  },
        'valueType':       { 'type': 'string'  },
        'maxValueCount':   { 'type': 'integer' },
        'options':         { 'type': 'array'   },
        'required':        { 'type': 'boolean' }
      },
      'required': [ 'uniqueId', 'name', 'valueType', 'required' ]
    };

    function AnnotationType(obj) {
      var defaults = {
        uniqueId:      '',
        name:          '',
        description:   null,
        valueType:     '',
        maxValueCount: null,
        options:       [],
        required:      false
      };

      obj = obj || {};
      _.extend(this, defaults, _.pick(obj, _.keys(defaults)));
    }

    AnnotationType.valid = function (obj) {
      return tv4.validate(obj, schema);
    };

    AnnotationType.create = function (obj) {
      if (!tv4.validate(obj, schema)) {
        throw new Error('invalid object from server: ' + tv4.error);
      }
      return new AnnotationType(obj);
    };

    AnnotationType.prototype.isValueTypeText = function () {
      return (this.valueType === AnnotationValueType.TEXT());
    };

    AnnotationType.prototype.isValueTypeNumber = function () {
      return (this.valueType === AnnotationValueType.NUMBER());
    };

    AnnotationType.prototype.isValueTypeDateTime = function () {
      return (this.valueType === AnnotationValueType.DATE_TIME());
    };

    AnnotationType.prototype.isValueTypeSelect = function () {
      return (this.valueType === AnnotationValueType.SELECT());
    };

    AnnotationType.prototype.isSingleSelect = function () {
      return (this.valueType === AnnotationValueType.SELECT()) &&
        (this.maxValueCount === AnnotationMaxValueCount.SELECT_SINGLE());
    };

    AnnotationType.prototype.isMultipleSelect = function () {
      return (this.valueType === AnnotationValueType.SELECT()) &&
        (this.maxValueCount === AnnotationMaxValueCount.SELECT_MULTIPLE());
    };

    /**
     * Returns true if the maxValueCount value is valid.
     */
    AnnotationType.prototype.isMaxValueCountValid = function () {
      if (this.isValueTypeSelect()) {
        return (this.isSingleSelect() || this.isMultipleSelect());
      }
      return ((this.maxValueCount === null) ||
              (this.maxValueCount === AnnotationMaxValueCount.NONE()));
    };

    /**
     * Called when the annotation type's value type has been changed.
     */
    AnnotationType.prototype.valueTypeChanged = function () {
      if (!this.isValueTypeSelect()) {
        this.maxValueCount = null;
      }
      this.options = [];
    };

    /**
     * Used to add an option. Should only be called when the value type is 'Select'.
     */
    AnnotationType.prototype.addOption = function () {
      if (!this.isValueTypeSelect()) {
        throw new Error('value type is not select: ' + this.valueType);
      }
      this.options.push('');
    };

    /**
     * Used to remove an option. Should only be called when the value type is 'Select'.
     */
    AnnotationType.prototype.removeOption = function (option) {
      if (this.options.length <= 1) {
        throw new Error('options is empty, cannot remove any more options');
      }
      this.options = _.without(this.options, option);
    };

    return AnnotationType;
  }

  return AnnotationTypeFactory;
});
