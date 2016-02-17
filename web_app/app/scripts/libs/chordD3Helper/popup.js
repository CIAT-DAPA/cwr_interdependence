/*
 * Crop Wild Relatives
 * https://github.com/CIAT-DAPA/cwr_interdependence
 *
 * Copyright (c) 2015 interdependence Steven Sotelo
  */

var CircosPopup = CircosPopup || {};

// General vars to config the popup
CircosPopup.config = {
    element : 'body',
    content : {        
        title_row : null,
        title_content : null,
        desc_row : null,
        desc_content: null
    }
};


CircosPopup.tools = {
    // Funtion to initialize the container for the popup
    init: function(){
        // Title
        CircosPopup.config.content.title_row = d3.select(CircosPopup.config.element).append('div')
            .classed('row', true);
        CircosPopup.config.content.title_content = CircosPopup.config.content.title_row.append('div')
            .attr({ class:'.col-md-6 .col-md-offset-3' });
        
        // Description
        CircosPopup.config.content.desc_row = d3.select(CircosPopup.config.element).append('div')
            .classed('row', true);
        CircosPopup.config.content.desc_content = CircosPopup.config.content.desc_row.append('div')
            .attr({ class:'.col-md-6 .col-md-offset-3' });
    },
    // Print a title in the popup
    print_title : function(text){
        CircosPopup.config.content.title_content.html('<br />' +text);
    },
    // Print a description in the popup
    print_desc : function(text){
        CircosPopup.config.content.desc_content.html(text);
    },
    // Clear all content in the popup
    clear : function (){
        CircosPopup.config.content.title_content.html('');
        CircosPopup.config.content.desc_content.html('');
    }
};