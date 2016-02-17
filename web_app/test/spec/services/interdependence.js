'use strict';

describe('Service: Interdependence', function () {

  // load the service's module
  beforeEach(module('cwrInterdependenceApp'));

  // instantiate service
  var Interdependence;
  beforeEach(inject(function (_Interdependence_) {
    Interdependence = _Interdependence_;
  }));

  it('should do something', function () {
    expect(!!Interdependence).toBe(true);
  });

});
