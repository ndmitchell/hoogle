
var instant = false; // should we search on key presses


/////////////////////////////////////////////////////////////////////
// SEARCHING

var currentSearch; // String
var oldSearches = cache(100);

$(function(){
    currentSearch = $("#hoogle").focus().keyup(searchBoxChange).val();
});

function searchBoxChange()
{
    if (!instant) return;
    var txt = $("#hoogle");
    var bod = $("#body");
    var now = txt.val();
    if (now == currentSearch) return; else currentSearch = now;
    var old = oldSearches.ask(now);
    if (old != undefined)
        bod.html(old);
    else
    {
        $.ajax({
            url: '?',
            data: {mode:'ajax', hoogle:now},
            dataType: 'html',
            complete: function(s){return function(e){
                if (e.status == 200) {
                    oldSearches.add(s,e.responseText);
                    if (txt.val() == s)
                        bod.html(e.responseText);
                } else
                    bod.html("<h1>Error: status " + e.status + "</h1><p>" + e.responseText + "</p>");
            }}(now)
        });
    }
}


/////////////////////////////////////////////////////////////////////
// INSTANT

$(function(){
    setInstant($.getQueryString('ajax') == "1" || $.cookie("instant") == "1");
    $("#instant").css("display","");
});

function setInstant(x)
{
    instant = x == undefined ? !instant : x ? true : false;
    $("#instantVal").html(instant ? "on" : "off");
    if (instant)
    {
        $.cookie("instant","1",{expires:365});
        searchBoxChange();
    }
    else
        $.cookie("instant",null);
}


/////////////////////////////////////////////////////////////////////
// SEARCH PLUGIN

$(function(){
    if (window.external && ("AddSearchProvider" in window.external))
        $("#plugin").css("display","");
});

function searchPlugin()
{
    var l = document.location;
    var url = l.protocol + "//" + l.hostname + l.pathname + $("link[rel=search]").attr("href");
    window.external.AddSearchProvider(url);
}


/////////////////////////////////////////////////////////////////////
// DOCUMENTATION

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
