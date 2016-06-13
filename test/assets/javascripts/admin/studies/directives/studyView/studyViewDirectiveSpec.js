/**
 * Jasmine test suite
 *
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2016 Canadian BioSample Repository (CBSR)
 */
define([
  'angular',
  'angularMocks',
  'underscore'
], function(angular, mocks, _) {
  'use strict';

  describe('Directive: studyViewDirective', function() {

    var createController = function () {
      this.element = angular.element('<study-view study="vm.study"></study-view>');
      this.scope = this.$rootScope.$new();
      this.scope.vm = { study: this.study };

      this.$compile(this.element)(this.scope);
      this.scope.$digest();
      this.controller = this.element.controller('studyView');
    };

    beforeEach(mocks.module('biobankApp', 'ui.router', 'biobank.test', function($provide) {
      $provide.value('$window', {
        localStorage: {
          setItem: jasmine.createSpy('mockWindowService.setItem'),
          getItem: jasmine.createSpy('mockWindowService.getItem')
        }
      });
    }));

    beforeEach(inject(function($state, testSuiteMixin, testUtils) {
      var self = this;

      _.extend(self, testSuiteMixin);

      self.injectDependencies('$rootScope',
                              '$compile',
                              '$window',
                              '$state',
                              'Study',
                              'factory');

      self.study = new self.Study(self.factory.study());

      self.putHtmlTemplates(
        '/assets/javascripts/admin/studies/directives/studyView/studyView.html');
    }));

    it('should contain a valid study', function() {
      createController.call(this);
      expect(this.controller.study).toBe(this.study);
    });

    it('should contain initialized tabs', function() {
      createController.call(this);
      expect(this.controller.tabs).toBeArrayOfSize(4);
    });

    it('should contain initialized local storage', function() {
      createController.call(this);

      expect(this.$window.localStorage.setItem)
        .toHaveBeenCalledWith('study.panel.processingTypes', true);
      expect(this.$window.localStorage.setItem)
        .toHaveBeenCalledWith('study.panel.specimenLinkAnnotationTypes', true);
      expect(this.$window.localStorage.setItem)
        .toHaveBeenCalledWith('study.panel.specimenLinkTypes', true);
    });

    it('should initialize the tab of the current state', function() {
      var tab;

      this.$state.current.name = 'home.admin.studies.study.processing';
      createController.call(this);

      tab = _.findWhere(this.controller.tabs, { heading: 'Processing' });

      expect(tab.active).toBe(true);
    });

  });

});
