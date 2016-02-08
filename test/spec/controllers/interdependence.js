'use strict';

describe('Controller: InterdependencectrlCtrl', function () {

  // load the controller's module
  beforeEach(module('cwrInterdependenceApp'));

  var InterdependencectrlCtrl,
    scope;

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    InterdependencectrlCtrl = $controller('InterdependencectrlCtrl', {
      $scope: scope
    });
  }));

  it('should attach a list of awesomeThings to the scope', function () {
    expect(scope.awesomeThings.length).toBe(3);
  });
});
