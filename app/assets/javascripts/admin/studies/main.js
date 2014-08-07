/**
 * Study package module.
 * Manages all sub-modules so other RequireJS modules only have to import the package.
 */
define([
  'angular',
  './states',
  './controllers',
  './services/services',
  './services/helpers',
  './directives/studyForms',
  './directives/studyPanels',
  './participants/main',
  './specimenGroups/main',
  './ceventTypes/main',
  './processing/main'
], function(angular) {
  'use strict';

  return angular.module('biobank.admin.studies', [
    'ngCookies',
    'ngRoute',
    'ui.bootstrap',
    'ngTable',
    'admin.studies.controllers',
    'admin.studies.states',
    'admin.studies.directives.studyForms',
    'admin.studies.directives.studyPanels',
    'studies.services',
    'admin.studies.helpers',
    'admin.studies.participants',
    'admin.studies.specimenGroups',
    'admin.studies.ceventTypes',
    'admin.studies.processing']);
});