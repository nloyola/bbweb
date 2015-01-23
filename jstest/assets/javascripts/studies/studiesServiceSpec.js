// Jasmine test suite
//
define(['angular', 'angularMocks', 'underscore', 'jquery', 'biobankApp'], function(angular, mocks, _, $) {
  'use strict';

  describe('Service: studiesService', function() {

    var studiesService, httpBackend;
    var studyId = 'dummy-study-id';
    var studyNoId = {
      version:     1,
      timeAdded:   '2014-10-20T09:58:43-0600',
      name:        'ST1',
      description: 'test'
    };
    var study = angular.extend({id: 'dummy-id'}, studyNoId);

    function uri(studyId) {
      var result = '/studies';
      if (arguments.length > 0) {
        result += '/' + studyId;
      }
      return result;
    }

    beforeEach(mocks.module('biobankApp'));

    beforeEach(inject(function (_studiesService_, $httpBackend) {
      studiesService = _studiesService_;
      httpBackend = $httpBackend;
    }));

    afterEach(function() {
      httpBackend.verifyNoOutstandingExpectation();
      httpBackend.verifyNoOutstandingRequest();
    });

    it('should have the following functions', function () {
      expect(angular.isFunction(studiesService.getStudyCount)).toBe(true);
      expect(angular.isFunction(studiesService.getStudies)).toBe(true);
      expect(angular.isFunction(studiesService.get)).toBe(true);
      expect(angular.isFunction(studiesService.addOrUpdate)).toBe(true);
      expect(angular.isFunction(studiesService.enable)).toBe(true);
      expect(angular.isFunction(studiesService.disable)).toBe(true);
      expect(angular.isFunction(studiesService.retire)).toBe(true);
      expect(angular.isFunction(studiesService.unretire)).toBe(true);
      expect(angular.isFunction(studiesService.processingDto)).toBe(true);
    });

    it('calling getStudyCount has valid URL', function() {
      httpBackend.whenGET(uri() + '/count').respond({
        status: 'success',
        data: [study]
      });

      studiesService.getStudyCount().then(function(data) {
        expect(data.length).toEqual(1);
        expect(_.isEqual(study, data[0]));
      });

      httpBackend.flush();
    });

    it('calling getStudies with no parameters has no query string', function() {
      httpBackend.whenGET(uri()).respond({
        status: 'success',
        data: [study]
      });

      studiesService.getStudies().then(function(data) {
        expect(data.length).toEqual(1);
        expect(_.isEqual(study, data[0]));
      });

      httpBackend.flush();
    });

    it('calling getStudies with filter parameter has valid query string', function() {
      var sortField = 'sortField';
      var url = uri() + '?' + $.param({sort: sortField});
      httpBackend.whenGET(url).respond({
        status: 'success',
        data: [study]
      });

      studiesService.getStudies({sort: sortField}).then(function(data) {
        expect(data.length).toEqual(1);
        expect(_.isEqual(study, data[0]));
      });

      httpBackend.flush();
    });

    it('calling getStudies with filter and status parameters has valid query string', function() {
      var nameFilter = 'nameFilter';
      var order = 'disabled';
      var url = uri() + '?' + $.param({
        filter: nameFilter,
        order: order
      });
      httpBackend.whenGET(url).respond({
        status: 'success',
        data: [study]
      });

      studiesService.getStudies({filter: nameFilter, order: order}).then(function(data) {
        expect(data.length).toEqual(1);
        expect(_.isEqual(study, data[0]));
      });

      httpBackend.flush();
    });

    it('calling getStudies with page and pageSize parameters has valid query string', function() {
      var page = 1;
      var pageSize = 5;
      var url = uri() + '?' + $.param({
        page: page,
        pageSize: pageSize
      });
      httpBackend.whenGET(url).respond({
        status: 'success',
        data: [study]
      });

      studiesService.getStudies({page: page, pageSize: pageSize}).then(function(data) {
        expect(data.length).toEqual(1);
        expect(_.isEqual(study, data[0]));
      });

      httpBackend.flush();
    });

    it('calling getStudies with sortField and order parameters has valid query string', function() {
      var sortField = 'name';
      var order = 'ascending';
      var url = uri() + '?' + $.param({
        sort: sortField,
        order: order
      });
      httpBackend.whenGET(url).respond({
        status: 'success',
        data: [study]
      });

      studiesService.getStudies({sort: sortField, order: order}).then(function(data) {
        expect(data.length).toEqual(1);
        expect(_.isEqual(study, data[0]));
      });

      httpBackend.flush();
    });

    it('get should return valid object', function() {
      httpBackend.whenGET(uri(study.id)).respond({
        status: 'success',
        data: study
      });

      studiesService.get(study.id).then(function(data) {
        expect(_.isEqual(study, data));
      });

      httpBackend.flush();
    });

    it('should allow adding a study', function() {
      var expectedResult = {status: 'success', data: 'success'};
      var cmd = {
        studyId:            study.studyId,
        name:               study.name,
        description:        study.description
      };
      httpBackend.expectPOST(uri(), cmd).respond(201, expectedResult);
      studiesService.addOrUpdate(studyNoId).then(function(reply) {
        expect(reply).toEqual('success');
      });
      httpBackend.flush();
    });

    it('should allow updating a study', function() {
      var expectedResult = {status: 'success', data: 'success'};
      var cmd = {
        id:                 study.id,
        expectedVersion:    study.version,
        studyId:            study.studyId,
        name:               study.name,
        description:        study.description
      };
      httpBackend.expectPUT(uri(study.id), cmd).respond(201, expectedResult);
      studiesService.addOrUpdate(study).then(function(reply) {
        expect(reply).toEqual('success');
      });
      httpBackend.flush();
    });

    function studyStatusChange(status, serviceFn) {
      var expectedCmd = { id: study.id, expectedVersion: study.version};
      var expectedResult = {status: 'success', data: 'success'};;
      httpBackend.expectPOST(uri(study.id) + '/' + status, expectedCmd).respond(201, expectedResult);
      serviceFn(study).then(function(reply) {
        expect(reply).toEqual('success');
      });
      httpBackend.flush();
    }

    it('should allow enabling a study', function() {
      studyStatusChange('enable', studiesService.enable);
    });

    it('should allow disabling a study', function() {
      studyStatusChange('disable', studiesService.disable);
    });

    it('should allow retiring a study', function() {
      studyStatusChange('retire', studiesService.retire);
    });

    it('should allow unretiring a study', function() {
      studyStatusChange('unretire', studiesService.unretire);
    });

    it('collectionDto should return valid object', function() {
      httpBackend.whenGET(uri(study.id) + '/dto/collection').respond({
        status: 'success',
        data: 'success'
      });

      studiesService.collectionDto(study.id).then(function(data) {
        expect(data).toBe('success');
      });

      httpBackend.flush();
    });

    it('processingDto should return valid object', function() {
      httpBackend.whenGET(uri(study.id) + '/dto/processing').respond({
        status: 'success',
        data: 'success'
      });

      studiesService.processingDto(study.id).then(function(data) {
        expect(data).toBe('success');
      });

      httpBackend.flush();
    });

    it('shoud get all value types', function() {
      httpBackend.whenGET(uri() + '/valuetypes').respond({
        status: 'success',
        data: 'success'
      });

      studiesService.valueTypes().then(function(data) {
        expect(data).toBe('success');
      });

      httpBackend.flush();
    });

  });

});
