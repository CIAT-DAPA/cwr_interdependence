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
      .when('/about', {
        templateUrl: 'views/about.html',
        controller: 'AboutCtrl'
      })
      .otherwise({
        redirectTo: '/'
      });
  });
