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
    element_description : '#description'
};


CircosDescription.tools = {
    // Print a title in the description
    print_content : function(title,brief,description){
        $(CircosDescription.config.element_title).html('<h1>' + title + '</h1>');
        $(CircosDescription.config.element_brief).html('<p>' + brief + '</p>');
        $(CircosDescription.config.element_description).html('<p>' + description + '</p>');
    },
    // Clear all content in the description
    clear : function (){
        $(CircosDescription.config.element_title).html('');
        $(CircosDescription.config.element_brief).html('');
        $(CircosDescription.config.element_description).html('');
    }
};