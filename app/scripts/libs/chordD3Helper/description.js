/*
 * Crop Wild Relatives
 * https://github.com/CIAT-DAPA/cwr_interdependence
 *
 * Copyright (c) 2015 interdependence Steven Sotelo
  */

var CircosDescription = CircosDescription || {};

// General vars to config the popup
CircosDescription.config = {
    element_title : '#title',
    element_brief : '#brief',
    content : {        
        title_row : null,
        title_content : null,
        desc_row : null,
        desc_content: null
    }
};


CircosDescription.tools = {
    // Funtion to initialize the container for the popup
    init: function(){
        // Title
        CircosDescription.config.content.title_row = d3.select(CircosDescription.config.element).append('div')
            .classed('row', true);
        CircosDescription.config.content.title_content = POPUP.config.content.title_row.append('div')
            .attr({ class:'.col-md-6 .col-md-offset-3' });
        
        // Description
        CircosDescription.config.content.desc_row = d3.select(CircosDescription.config.element).append('div')
            .classed('row', true);
        CircosDescription.config.content.desc_content = CircosDescription.config.content.desc_row.append('div')
            .attr({ class:'.col-md-6 .col-md-offset-3' });
    },
    // Print a title in the popup
    print_title : function(text){
        CircosDescription.config.content.title_content.html('<h1>' + text + '</h1>');
    },
    // Print a description in the popup
    print_desc : function(text){
        CircosDescription.config.content.desc_content.html(text);
    },
    // Clear all content in the popup
    clear : function (){
        CircosDescription.config.content.title_content.html('');
        CircosDescription.config.content.desc_content.html('');
    }
};