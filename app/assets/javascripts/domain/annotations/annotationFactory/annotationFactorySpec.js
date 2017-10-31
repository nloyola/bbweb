/**
 * Jasmine test suite
 *
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2016 Canadian BioSample Repository (CBSR)
 */
/* global angular */

import _ from 'lodash';
import ngModule from '../../index'

/*
 * AnnotationSpec.js has test cases for all types of annotations.
 *
 * These test cases provide additional code coverage to the ones in AnnotationSpec.js.
 */
describe('MultipleSelectAnnotation', function() {

  beforeEach(() => {
    angular.mock.module(ngModule, 'biobank.test');
    angular.mock.inject(function(EntityTestSuiteMixin) {
      _.extend(this, EntityTestSuiteMixin);
      this.injectDependencies('annotationFactory',
                              'AnnotationType',
                              'AnnotationValueType',
                              'AnnotationMaxValueCount',
                              'Factory');
    });
  });

  it('cannot be created with invalid selected values', function() {
    var jsonAnnotationType = this.Factory.annotationType({
          valueType:     this.AnnotationValueType.SELECT,
          maxValueCount: this.AnnotationMaxValueCount.SELECT_MULTIPLE,
          options:       [ 'option1', 'option2' ],
          required:      true
        }),
        annotationType = new this.AnnotationType(jsonAnnotationType),
        jsonAnnotation = this.Factory.annotation({ selectedValues: [ this.Factory.stringNext() ] },
                                                 jsonAnnotationType);

    expect(() => {
      this.annotationFactory.create(jsonAnnotation, annotationType);
    }).toThrowError('invalid selected values in object from server');
  });

});
