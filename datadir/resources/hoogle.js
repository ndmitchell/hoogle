
var useAjax = false;

$(function(){
    if (window.external && ("AddSearchProvider" in window.external))
        document.getElementById("plugin").style.display = "";

    $("#hoogle").focus();
    if (useAjax)
    {
        $("#hoogle").keyup(function(){
            $.ajax({
                url: '?',
                data: {mode:'ajax', 'hoogle':$("#hoogle").val()},
                dataType: 'html',
                complete: function(e){
                    $("#body").html(e.responseText);
                }
            });
        });
    }
});

function searchPlugin()
{
    window.external.AddSearchProvider("http://haskell.org/hoogle/res/search.xml");
}

function docs(i)
{
    var e = document.getElementById("d" + i);
    e.className = (e.className == "shut" ? "open" : "shut");
    return false;
}
