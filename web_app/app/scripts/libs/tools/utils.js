/* Script*/
var Tools = Tools || {}

Tools.utils = {
    capitalLetter: function (string) {
        return string.charAt(0).toUpperCase() + string.slice(1);
    }
}

String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.split(search).join(replacement);
};

$(".nav a").on("click", function(){
   $(".nav").find(".active").removeClass("active");
   $(this).parent().addClass("active");
   $(this).css("font-weight","bold");
});
