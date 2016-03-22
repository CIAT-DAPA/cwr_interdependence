'use strict';

/**
 * @ngdoc service
 * @name cwrInterdependenceApp.Interdependence
 * @description
 * # Interdependence
 * Service in the cwrInterdependenceApp.
 */
angular.module('cwrInterdependenceApp')
  .service('Interdependence', function () {
    // AngularJS will instantiate a singleton by calling "new" on this function
    this._source = '';
    this._precision = 0;
    
     
    var aLittleBit = Math.PI / 100000;
    
    this.setSource = function(path){
        this._source = path;
    }
    
    this.setPrecision = function(precision){
        this._precision = precision;
    }
    
    this.draw = function(){
        var precision = this._precision;
        d3.json(this._source, function (data) {
            
            CircosPopup.config.element = '#popup';
            CircosPopup.tools.init();
            
            var chart = Globalmigration.chart(data, {
                element: '#diagram',
                openSubgroup: 'close',
                animationDuration: 500,
                width: 900,
                height: 800,                
                margin: 100,
                arcPadding: 0.01,
                precision: precision,
                layout: {
                    alpha: 0,
                    threshold: 0.1,
                    labelThreshold: 0.1,
                    //colors: 'cd3d08 ec8f00 6dae29 683f92 b60275 2058a5 00a592 009d3c 378974 ffca00'.split(' ').map(function (c) { return '#' + c; })
                    //colors:  'FF0000 FF4500 EE4000 CD3700 CD0000 8B0000 A2CD5A 66CD00 458B00 228B22 006400 EED5B7 CDAA7D 8B7355 8B4513 EEC900 00BFFF 1E90FF 1C86EE 104E8B 0000CD FFA500 FF8C00'.split(' ').map(function (c) { return '#' + c; })
                    colors: Tools.vars.color_chord
                }
            },{
                popup:'open',
                description:'open'
            });

            Globalmigration.timeline(chart, { element: '#timeline', incr: 1 });

            chart.draw();
        });
    }    
    
  });
