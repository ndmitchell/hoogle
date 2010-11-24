
$(function(){
    if (window.external && ("AddSearchProvider" in window.external))
        document.getElementById("plugin").style.display = "";

    $("#hoogle").focus();
    if ($.getQueryString('ajax'))
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


// From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
;(function ($) {
    $.extend({      
        getQueryString: function (name) {           
            function parseParams() {
                var params = {},
                    e,
                    a = /\+/g,  // Regex for replacing addition symbol with a space
                    r = /([^&=]+)=?([^&]*)/g,
                    d = function (s) { return decodeURIComponent(s.replace(a, " ")); },
                    q = window.location.search.substring(1);

                while (e = r.exec(q))
                    params[d(e[1])] = d(e[2]);

                return params;
            }

            if (!this.queryStringParams)
                this.queryStringParams = parseParams(); 

            return this.queryStringParams[name];
        }
    });
})(jQuery);
