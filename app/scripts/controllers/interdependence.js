'use strict';

/**
 * @ngdoc function
 * @name cwrInterdependenceApp.controller:InterdependencectrlCtrl
 * @description
 * # InterdependencectrlCtrl
 * Controller of the cwrInterdependenceApp
 */
angular.module('cwrInterdependenceApp')
  .controller('InterdependenceCtrl',function ($scope,$routeParams,Interdependence) {
      $scope.diagram_interdependence = Interdependence;      
      $scope.diagram_interdependence.setSource('data/json/' + $routeParams.type + '.json');
      $scope.diagram_interdependence.draw();
  });
