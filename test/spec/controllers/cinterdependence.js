'use strict';

describe('Controller: CinterdependenceCtrl', function () {

  // load the controller's module
  beforeEach(module('cwrInterdependenceApp'));

  var CinterdependenceCtrl,
    scope;

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    CinterdependenceCtrl = $controller('CinterdependenceCtrl', {
      $scope: scope
    });
  }));

  it('should attach a list of awesomeThings to the scope', function () {
    expect(scope.awesomeThings.length).toBe(3);
  });
});
