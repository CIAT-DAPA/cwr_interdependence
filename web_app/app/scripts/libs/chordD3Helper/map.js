/*
 * Crop Wild Relatives
 * https://github.com/CIAT-DAPA/cwr_interdependence
 *
 * Copyright (c) 2015 interdependence Steven Sotelo
  */

var CircosMap = CircosMap || {};

// General vars to config the popup
CircosMap.config = {
    element_content: 'map',
    map: null,
    defaultFill: "#ABDDA4",
    selectedFill: "#fa0fa0"
};


CircosMap.tools = {
    // Print a title in the description
    init: function () {
        CircosMap.config.map = new Datamap({ element: document.getElementById(CircosMap.config.element_content),
                                                //projection: 'mercator',
                                                fills: {
                                                    defaultFill: CircosMap.config.defaultFill,
                                                    selectedFill: CircosMap.config.selectedFill
                                                }
                                           });
    },
    changeColorCountries: function (countries) {
        var choropleth = {};
        for(var i=0; i<countries.length;i++)
            choropleth[countries[i]] = { fillKey: 'selectedFill' }
        CircosMap.config.map.updateChoropleth(choropleth,{reset:true});
    }
};