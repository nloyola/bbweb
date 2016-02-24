/**
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2015 Canadian BioSample Repository (CBSR)
 */
// Jasmine test suite
//
define([
  'angular',
  'angularMocks',
  'underscore',
  'biobankApp'
], function(angular, mocks, _) {
  'use strict';

  describe('SpecimenGroupViewer', function() {

    var SpecimenGroupViewer, jsonEntities, centre;

    beforeEach(mocks.module('biobankApp', 'biobank.test'));

    beforeEach(inject(function(_SpecimenGroupViewer_,
                               jsonEntities) {
      SpecimenGroupViewer = _SpecimenGroupViewer_;
      jsonEntities   = jsonEntities;

      centre = jsonEntities.centre();
    }));

    it('should open a modal when created', inject(function (testUtils) {
      var count = 0,
          modal = this.$injector.get('$uibModal'),
          study,
          specimenGroup,
          viewer;

      spyOn(modal, 'open').and.callFake(function () {
        return testUtils.fakeModal();
      });

      // jshint unused:false
      study = jsonEntities.study();
      specimenGroup = jsonEntities.specimenGroup(study);
      viewer = new SpecimenGroupViewer(specimenGroup);

      expect(modal.open).toHaveBeenCalled();
    }));

    it('should display valid attributes', function() {
      var EntityViewer = this.$injector.get('EntityViewer');
      var attributes, study, specimenGroup, viewer;

      spyOn(EntityViewer.prototype, 'addAttribute').and.callFake(function (label, value) {
        attributes.push({label: label, value: value});
      });

      attributes = [];
      study = jsonEntities.study();
      specimenGroup = jsonEntities.specimenGroup(study);
      viewer = new SpecimenGroupViewer(specimenGroup);

      expect(attributes).toBeArrayOfSize(7);

      _.each(attributes, function(attr) {
        switch (attr.label) {
        case 'Name':
          expect(attr.value).toBe(specimenGroup.name);
          break;
        case 'Units':
          expect(attr.value).toBe(specimenGroup.units);
          break;
        case 'Anatomical Source':
          expect(attr.value).toBe(specimenGroup.anatomicalSourceType);
          break;
        case 'Preservation Type':
          expect(attr.value).toBe(specimenGroup.preservationType);
          break;
        case 'Preservation Temperature':
          expect(attr.value).toBe(specimenGroup.preservationTemperatureType);
          break;
        case 'Specimen Type':
          expect(attr.value).toBe(specimenGroup.specimenType);
          break;
        case 'Description':
          expect(attr.value).toBe(specimenGroup.description);
          break;
        default:
          jasmine.getEnv().fail('label is invalid: ' + attr.label);
        }
      });
    });

  });

});
