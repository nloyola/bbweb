/**
 * Jasmine test suite
 *
 */
/* global angular */

describe('states', function() {

  beforeEach(() => {
    angular.mock.module('biobankApp', 'biobank.test')
    angular.mock.inject(function(StateTestSuiteMixin) {
      Object.assign(this, StateTestSuiteMixin)

      this.injectDependencies('$q',
                              '$rootScope',
                              '$location',
                              '$httpBackend',
                              '$state');
    })
  })

  describe('when navigating to `/`', function () {

    it('should go to the home state', function() {
      this.gotoUrl('/');
      expect(this.$state.current.name).toBe('home');
    })

  })

  describe('when navigating to `/about`', function () {

    it('should go to the about state', function() {
      this.gotoUrl('/about');
      expect(this.$state.current.name).toBe('home.about');
    })

  })

  describe('when navigating to `/contact`', function () {

    it('should go to the contact state', function() {
      this.gotoUrl('/contact');
      expect(this.$state.current.name).toBe('home.contact');
    })

  })

  xdescribe('otherwise', function () {

    it('should go to the 404 state', function () {
      this.gotoUrl('/someNonExistentUrl');
      expect(this.$state.current.name).toEqual('404');
    });

    it('should not change the url', function () {
      var badUrl = '/someNonExistentUrl';
      this.goToUrl(badUrl);
      expect(this.$location.url()).toEqual(badUrl);
    });
  });

})