/*
 * Crop Wild Relatives
 * https://github.com/CIAT-DAPA/cwr_interdependence
 *
 * Copyright (c) 2015 interdependence Steven Sotelo
  */

var POPUP = POPUP || {};

// General vars to config the popup
POPUP.config = {
    element : 'body',
    content : {        
        title_row : null,
        title_content : null,
        desc_row : null,
        desc_content: null
    }
};


POPUP.tools = {
    // Funtion to initialize the container for the popup
    init: function(){
        // Title
        POPUP.config.content.title_row = d3.select(POPUP.config.element).append('div')
            .classed('row', true);
        POPUP.config.content.title_content = POPUP.config.content.title_row.append('div')
            .attr({ class:'.col-md-6 .col-md-offset-3' });
        
        // Description
        POPUP.config.content.desc_row = d3.select(POPUP.config.element).append('div')
            .classed('row', true);
        POPUP.config.content.desc_content = POPUP.config.content.desc_row.append('div')
            .attr({ class:'.col-md-6 .col-md-offset-3' });
    },
    // Print a title in the popup
    print_title : function(text){
        POPUP.config.content.title_content.html('<h1>' + text + '</h1>');
    },
    // Print a description in the popup
    print_desc : function(text){
        POPUP.config.content.desc_content.html(text);
    },
    // Clear all content in the popup
    clear : function (){
        POPUP.config.content.title_content.html('');
        POPUP.config.content.desc_content.html('');
    }
};