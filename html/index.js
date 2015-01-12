$(function (){
    for (var i = 0; i < tags.length; i++)
        $("#tag").append("<option>" + tags[i] + "</option>");
    $("#tag").chosen({width: "25%"});
});
