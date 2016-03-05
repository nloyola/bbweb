/**
 * Jasmine test suite
 *
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2016 Canadian BioSample Repository (CBSR)
 */
define([
  'angular',
  'angularMocks',
  'underscore',
  'biobankApp'
], function(angular, mocks, _) {
  'use strict';

  describe('ceventTypesAddAndSelectDirective', function() {

    beforeEach(mocks.module('biobankApp', 'biobank.test'));

    beforeEach(inject(function($rootScope, $compile, testUtils) {
      var self = this, jsonStudy, jsonCet;

      self.$q                   = self.$injector.get('$q');
      self.$state               = self.$injector.get('$state');
      self.Study                = self.$injector.get('Study');
      self.CollectionEventType  = self.$injector.get('CollectionEventType');
      self.jsonEntities         = self.$injector.get('jsonEntities');

      jsonStudy = self.jsonEntities.study();
      jsonCet   = self.jsonEntities.collectionEventType(jsonStudy);

      self.study = new self.Study(jsonStudy);
      self.collectionEventType = new self.CollectionEventType(jsonCet);
      self.createController = setupController();

      spyOn(self.CollectionEventType, 'list').and.returnValue(self.$q.when([ self.collectionEventType ]));
      spyOn(this.$state, 'go').and.callFake(function () {});

      testUtils.putHtmlTemplates(
        '/assets/javascripts/admin/directives/studies/ceventTypes/ceventTypesAddAndSelect/ceventTypesAddAndSelect.html');

      function setupController() {
        return create;

        function create() {
          self.element = angular.element([
            '<cevent-types-add-and-select',
            '   study="vm.study">',
            '</cevent-types-add-and-select>'
          ].join(''));
          self.scope = $rootScope.$new();
          self.scope.vm = { study: self.study };

          $compile(this.element)(this.scope);
          self.scope.$digest();
          self.controller = this.element.controller('ceventTypesAddAndSelect');
        }
      }
    }));

    it('has valid scope', function() {
      this.createController();
      expect(this.controller.study).toBe(this.study);
    });

    it('function add switches to correct state', function() {
      this.createController();
      this.controller.add();
      this.scope.$digest();
      expect(this.$state.go).toHaveBeenCalledWith('home.admin.studies.study.collection.ceventTypeAdd');
    });

    it('function select switches to correct state', function() {
      this.createController();
      this.controller.select(this.collectionEventType);
      this.scope.$digest();
      expect(this.$state.go).toHaveBeenCalledWith('home.admin.studies.study.collection.view',
                                                  { ceventTypeId: this.collectionEventType.id });
    });

    it('function recurring returns a valid result', function() {
      this.createController();
      this.collectionEventType.recurring = false;
      expect(this.controller.getRecurringLabel(this.collectionEventType)).toBe('Not recurring');
      this.collectionEventType.recurring = true;
      expect(this.controller.getRecurringLabel(this.collectionEventType)).toBe('Recurring');
    });

  });

});
