/**
 * Jasmine test suite
 *
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2015 Canadian BioSample Repository (CBSR)
 */
define(function (require) {
  'use strict';

  var angular                = require('angular'),
      mocks                  = require('angularMocks'),
      _                      = require('underscore'),
      entityUpdateSharedSpec = require('../../../../test/entityUpdateSharedSpec');

  describe('Directive: studySummaryDirective', function() {

    var createController = function () {
      this.element = angular.element('<study-summary study="vm.study"></study-summary>');
      this.scope = this.$rootScope.$new();
      this.scope.vm = { study: this.study };

      this.$compile(this.element)(this.scope);
      this.scope.$digest();
      this.controller = this.element.controller('studySummary');
    };

    beforeEach(mocks.module('biobankApp', 'biobank.test'));

    beforeEach(inject(function(testSuiteMixin, testUtils) {
      var self = this,
          ceventType;

      _.extend(self, testSuiteMixin);

      self.injectDependencies('$q',
                              '$rootScope',
                              '$compile',
                              '$state',
                              'Study',
                              'CollectionEventType',
                              'modalService',
                              'notificationsService',
                              'factory');

      ceventType = new self.CollectionEventType(self.factory.collectionEventType());

      spyOn(self.CollectionEventType, 'list').and.returnValue(self.$q.when([ ceventType ]));
      spyOn(self.modalService, 'showModal').and.returnValue(self.$q.when(true));

      self.study = new self.Study(self.factory.study());

      this.putHtmlTemplates(
        '/assets/javascripts/admin/studies/directives/studySummary/studySummary.html',
        '/assets/javascripts/common/directives/truncateToggle.html',
        '/assets/javascripts/admin/directives/statusLine/statusLine.html',
        '/assets/javascripts/common/modalInput/modalInput.html');
    }));

    it('should contain valid settings to display the study summary', function() {
      createController.call(this);
      expect(this.controller.study).toBe(this.study);
      expect(this.controller.descriptionToggleLength).toBeDefined();
      expect(this.controller.collectionEventTypes).toBeArrayOfSize(1);
      expect(this.controller.hasCollectionEventTypes).toBeTrue();
    });

    it('should have valid settings when study has no collection event types', function() {
      this.CollectionEventType.list = jasmine.createSpy().and.returnValue(this.$q.when([ ]));
      createController.call(this);
      expect(this.controller.collectionEventTypes).toBeEmptyArray();
      expect(this.controller.hasCollectionEventTypes).toBeFalse();
    });

    describe('updates to name', function () {

      var context = {};

      beforeEach(inject(function () {
        context.entity             = this.Study;
        context.createController   = createController;
        context.updateFuncName     = 'updateName';
        context.controllerFuncName = 'editName';
        context.modalInputFuncName = 'text';
      }));

      entityUpdateSharedSpec(context);

    });

    describe('updates to description', function () {

      var context = {};

      beforeEach(inject(function () {
        context.entity             = this.Study;
        context.createController   = createController;
        context.updateFuncName     = 'updateDescription';
        context.controllerFuncName = 'editDescription';
        context.modalInputFuncName = 'textArea';
      }));

      entityUpdateSharedSpec(context);

    });

    describe('enabling a study', function() {
      var context = {};

      beforeEach(inject(function () {
        context.status = 'enable';
      }));

      sharedStudyStatusBehaviour(context);
    });

    describe('disabling a study', function() {
      var context = {};

      beforeEach(inject(function () {
        context.status = 'disable';
      }));

      sharedStudyStatusBehaviour(context);
    });

    describe('retiring a study', function() {
      var context = {};

      beforeEach(inject(function () {
        context.status = 'retire';
      }));

      sharedStudyStatusBehaviour(context);
    });

    describe('unretiring a study', function() {
      var context = {};

      beforeEach(inject(function () {
        context.status = 'unretire';
      }));

      sharedStudyStatusBehaviour(context);
    });


    function sharedStudyStatusBehaviour(context) {

      describe('(shared) study status', function () {

        it('change status', function () {
          spyOn(this.Study, 'get').and.returnValue(this.$q.when(this.study));
          spyOn(this.Study.prototype, context.status).and.returnValue(this.$q.when(this.study));

          createController.call(this);
          this.controller.changeStatus(context.status);
          this.scope.$digest();
          expect(this.Study.prototype[context.status]).toHaveBeenCalled();
        });

      });
    }

    it('should throw error for when trying to change to an invalid status', function () {
      var self = this,
          badStatus = 'xxx';

      createController.call(this);
      expect(function () {
        self.controller.changeStatus(badStatus);
      }).toThrow(new Error('invalid status: ' + badStatus));
    });
  });

});
