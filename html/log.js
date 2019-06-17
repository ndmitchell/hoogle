var data = #{data};
function series(name, val)
{
    val.label = name;
    val.data = [];
    for (var i = 0; i < data.length; i++)
    {
        var x = data[i][name.toLowerCase()];
        val.data.push([new Date(data[i].date),x == 0 ? null : x]);
    }
    return val;
}
$(function(){
    var settings =
        {series: {lines: {show: true}, points: {show: true}}
        ,xaxis: {mode: "time", minTickSize: [1, "day"]}
        ,yaxes: [{}, {position: "right"}]
        ,grid: {hoverable: true, clickable: true}};
    $.plot("#users",[series("Uses",{yaxis:2}),series("Users",{})], settings);
    $.plot("#timings",[series("Average",{}),series("Slowest",{yaxis:2})], settings);
    settings.series.lines.show = false;
    $.plot("#errors",[series("Errors",{color:"darkred"})], settings);

    $(".plot").bind("plothover", function (event, pos, item) {
        if (item) {
            var x = new Date(item.datapoint[0]).toDateString();
            var y = item.datapoint[1];
            y = Math.floor(y) == y ? y : y.toFixed(3);

            $("#tooltip").html(y + " = " + item.series.label + " on " + x)
                .css({top: item.pageY+5, left: item.pageX+5})
                .fadeIn(200);
        } else {
            $("#tooltip").hide();
        }
    });

});
