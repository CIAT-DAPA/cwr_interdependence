/*
 * globalmigration
 * https://github.com/null2/globalmigration
 *
 * Copyright (c) 2013 null2 GmbH Berlin
 * Licensed under the MIT license.
 */

// Timeline: year selector
(function (scope) {
    scope.timeline = function (diagram, config) {
        var years = Object.keys(diagram.data.matrix).map(function (y) { return y; });

        config = config || {};
        config.element = config.element || 'body';
        config.now = config.now || years[0];
        config.incr = config.incr || 1; // year span for buttons

        var form = d3.select(config.element).append('form');

        var year = form.selectAll('.year')
            .data(years);

        var span = year.enter().append('span')
            .classed('year', true);
        
        
        
        span.append('input')
            .attr({
                name: 'year',
                type: 'button',
                class: 'btn btn-default timeline-button',
                id: function (d) { return 'year-' + d; },
                value: function (d) { return Tools.utils.capitalLetter(d.replace('_',' ')); },
                checked: function (d) {
                    var control='#year-' + d;
                    year.select(control).attr('class',d === config.now ? 'btn btn-success timeline-button':'btn btn-default timeline-button'); 
                    return d === config.now || null; }
            })
            .on('click', function (d) {
                var y = d;
                year.selectAll('input').attr('checked', function (d) {
                    var control='#year-' + d;
                    year.select(control).attr('class', y === d ? 'btn btn-success timeline-button':'btn btn-default timeline-button');
                    
                    return y === d || null;
                });
                diagram.draw(d);
            });
        
        

        /*span.append('label')
            .attr('for', function (d) { return 'year-' + d; })
            .text(function (d) { return "" + d + (config.incr === 1 ? "" : "-" + (d + config.incr)); });*/

        // keyboard control
        /*d3.select(document.body).on('keypress', function () {
            var idx = d3.event.which - 49;
            var y = years[idx];
            if (y) {
                year.selectAll('input').each(function (d) {
                    if (d === y) {
                        d3.select(this).on('click')(d);
                    }
                });
            }
        });*/
    };
})(window.Globalmigration || (window.Globalmigration = {}));
