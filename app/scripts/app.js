'use strict';

/**
 * @ngdoc overview
 * @name cwrInterdependenceApp
 * @description
 * # cwrInterdependenceApp
 *
 * Main module of the application.
 */
angular
  .module('cwrInterdependenceApp', [
    'ngAnimate',
    'ngCookies',
    'ngResource',
    'ngRoute',
    'ngSanitize',
    'ngTouch'
  ])
  .config(function ($routeProvider) {
    $routeProvider
      .when('/', {
        templateUrl: 'views/main.html',
        controller: 'MainCtrl'
      })
      .when('/map', {
        templateUrl: 'views/map.html',
        controller: 'MapCtrl'
      })
      .when('/interdependence/:type', {
        templateUrl: 'views/interdependence.html',
        controller: 'InterdependenceCtrl'
      })
      .when('/about', {
        templateUrl: 'views/about.html',
        controller: 'AboutCtrl'
      })
      .otherwise({
        redirectTo: '/'
      });
  });
