'use strict';

/**
 * @ngdoc function
 * @name cwrInterdependenceApp.controller:MainCtrl
 * @description
 * # MainCtrl
 * Controller of the cwrInterdependenceApp
 */
angular.module('cwrInterdependenceApp')
  .controller('MainCtrl', function ($scope) {
     var datafile = 'data/json/interdependence.json';
      var aLittleBit = Math.PI / 100000;

      d3.json(datafile, function(data) {
        var now = 2005;
        
        var chart = Globalmigration.chart(data, {
          element: '#diagram',
          now: now,
          animationDuration: 500,
          //margin: 125,
          margin: 125,
          arcPadding: 0.04,
          layout: {
            alpha: 0,
            threshold: 5,
            labelThreshold: 5,
            //colors: 'cd3d08 ec8f00 6dae29 683f92 b60275 2058a5 00a592 009d3c 378974 ffca00'.split(' ').map(function(c) { return '#' + c; })
          }
        });

        Globalmigration.timeline(chart, { now: now, element: '#timeline',incr: 1 });

        chart.draw(now);
      });
  });
