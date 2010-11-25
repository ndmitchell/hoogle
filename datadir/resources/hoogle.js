
$(function(){
    if (window.external && ("AddSearchProvider" in window.external))
        document.getElementById("plugin").style.display = "";

    $("#hoogle").focus();
    if ($.getQueryString('ajax'))
    {
        var c = cache(100);
        var last = $("#hoogle").val();

        $("#hoogle").keyup(function(){
            var now = $("#hoogle").val();
            if (now == last) return; else last = now;
            var old = c.ask(now);
            if (old != undefined)
                $("#body").html(old);
            else
            {
                $.ajax({
                    url: '?',
                    data: {mode:'ajax', hoogle:now},
                    dataType: 'html',
                    complete: function(s){return function(e){
                        c.add(s,e.responseText);
                        if ($("#hoogle").val() == s)
                            $("#body").html(e.responseText);
                    }}(now)
                });
            }
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


/////////////////////////////////////////////////////////////////////
// CACHE

function cache(maxElems)
{
    // FIXME: Currently does not evict things
    var contents = {}; // what we have in the cache

    return {
        add: function(key,val)
        {
            contents[key] = val;
        },
        
        ask: function(key)
        {
            return contents[key];
        }
    };
}



/////////////////////////////////////////////////////////////////////
// EXTERNAL jQUERY BITS

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
