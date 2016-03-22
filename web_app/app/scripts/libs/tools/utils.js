/* Script*/
var Tools = Tools || {}

Tools.utils = {
    capitalLetter: function (string) {
        return string.charAt(0).toUpperCase() + string.slice(1);
    },
    binary_search: function(array, item){
        var start = 0, end = array.length - 1, pos, value = -1;
        while (start <= end) {
            pos = parseInt((start+end) / 2);
            if ((array[pos] == item) || (pos < array.length &&  array[pos] <= item && array[pos+1] > item) ){                
                value = pos;   
                break;
            }
            else if ( array[pos] < item ) 
                start = pos+1;
            else 
                end = pos-1;
        }
        return value;
    }
}

Tools.vars = {
    color_chord : 'FF0000 FF4500 EE4000 CD3700 CD0000 8B0000 A2CD5A 66CD00 458B00 228B22 006400 EED5B7 CDAA7D 8B7355 8B4513 EEC900 00BFFF 1E90FF 1C86EE 104E8B 0000CD FFA500 FF8C00'.split(' ').map(function (c) { return '#' + c; }),
    color_map: function(){
        var list = {defaultFill: "#d9d9d9"};
        for(var i=0;i<Tools.vars.color_chord.length;i++)
            list[i.toString()]=Tools.vars.color_chord[i];
        return list;
    }
}

/* Script for replace all character in a string*/
String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    return target.split(search).join(replacement);
};

/* Function to change the styles of the navbar */
$(".nav a").on("click", function () {
    $(".nav").find(".active").removeClass("active");
    $(this).parent().addClass("active");
});
