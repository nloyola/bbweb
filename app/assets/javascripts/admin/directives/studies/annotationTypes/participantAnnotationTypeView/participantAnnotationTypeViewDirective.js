/**
 * @author Nelson Loyola <loyola@ualberta.ca>
 * @copyright 2016 Canadian BioSample Repository (CBSR)
 */
define(['underscore'], function (_) {
  'use strict';

  /**
   *
   */
  function participantAnnotationTypeViewDirective() {
    var directive = {
      restrict: 'E',
      scope: {},
      bindToController: {
        study:          '=',
        annotationType: '='
      },
      template: [
        '<annotation-type-view',
        '  study="vm.study"',
        '  annotation-type="vm.annotationType"',
        '  return-state="home.admin.studies.study.participants"',
        '  on-update="vm.onUpdate">',
        '</annotation-type-view>'
      ].join(''),
      controller: ParticipantAnnotationTypeViewCtrl,
      controllerAs: 'vm'
    };

    return directive;
  }

  ParticipantAnnotationTypeViewCtrl.$inject = [
    '$q', 'notificationsService'
  ];

  function ParticipantAnnotationTypeViewCtrl($q, notificationsService) {
    var vm = this;

    vm.onUpdate = onUpdate;

    function onUpdate(annotationType) {
      return vm.study.updateAnnotationType(annotationType)
        .then(postUpdate)
        .then(notifySuccess)
        .catch(notificationsService.updateError);
    }

    function postUpdate(study) {
      var deferred = $q.defer();

      vm.study = study;
      vm.annotationType = _.findWhere(vm.study.annotationTypes,
                                      { uniqueId: vm.annotationType.uniqueId });
      if (_.isUndefined(vm.annotationType)) {
        deferred.reject('could not update annotation type');
      } else {
        deferred.resolve(true);
      }
      return deferred.promise;
    }

    function notifySuccess() {
      return notificationsService.success(
        'Annotation type changed successfully.',
        'Change successful',
        1500);
    }

  }

  return participantAnnotationTypeViewDirective;

});
