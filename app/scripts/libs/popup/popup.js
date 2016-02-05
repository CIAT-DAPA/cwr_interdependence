/*
 * Crop Wild Relatives
 * https://github.com/CIAT-DAPA/cwr_interdependence
 *
 * Copyright (c) 2015 interdependence Steven Sotelo
  */

var POPUP = POPUP || {};

POPUP.config = {
    element : 'body',
    content : {
        container: null,
        title_row : null,
        title_content : null,
        desc_row : null,
        desc_content: null
    }
};

POPUP.tools = {
    init: function(){
       POPUP.config.content.container = d3.select(POPUP.config.element).append('div')
                .classed('container', true);

        // Title
        POPUP.config.content.title_row = POPUP.config.content.container.append('div')
            .classed('row', true);
        POPUP.config.content.title_content = POPUP.config.content.title_row.append('div').attr({
            class:'.col-md-6 .col-md-offset-3'
        });
        
        // Description
        POPUP.config.content.desc_row = POPUP.config.content.container.append('div')
            .classed('row', true);
        POPUP.config.content.desc_content = POPUP.config.content.desc_row.append('div').attr({
            class:'.col-md-6 .col-md-offset-3'
        });
    },
    print_title : function(text){
        POPUP.config.content.title_content.html('<h1>' + text + '</h1>');
    },
    print_desc : function(text){
        POPUP.config.content.desc_content.html(text);
    }
};