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
        var datafile = 'data/json/fs_interdependence.json';
        var aLittleBit = Math.PI / 100000;

        d3.json(datafile, function (data) {

            var chart = Globalmigration.chart(data, {
                element: '#diagram',
                animationDuration: 500,
                width: 900,
                openSubgroup: 'close',
                height: 900,
                margin: 100,
                arcPadding: 0.01,
                layout: {
                    alpha: 0,
                    threshold: 1,
                    labelThreshold: 1,
                    //colors: 'cd3d08 ec8f00 6dae29 683f92 b60275 2058a5 00a592 009d3c 378974 ffca00'.split(' ').map(function (c) { return '#' + c; })
                    colors: 'FF0000 FF4500 EE4000 CD3700 CD0000 8B0000 A2CD5A 66CD00 458B00 228B22 006400 EED5B7 CDAA7D 8B7355 8B4513 EEC900 00BFFF 1E90FF 1C86EE 104E8B 0000CD FFA500 FF8C00'.split(' ').map(function (c) { return '#' + c; })
                }
            });

            Globalmigration.timeline(chart, { element: '#timeline', incr: 1 });

            chart.draw();
        });
    });
