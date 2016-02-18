/* Script*/
var Tools = Tools || {}

Tools.utils = {
    capitalLetter: function (string) {
        return string.charAt(0).toUpperCase() + string.slice(1);
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
