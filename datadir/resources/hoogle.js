
var client = false; // are we running as an embedded search box
var instant = false; // should we search on key presses


/////////////////////////////////////////////////////////////////////
// SEARCHING

var currentSearch; // String
var oldSearches = cache(100);
var timeoutId;

$(function(){
    var txt = $("#hoogle");
    client = !txt.hasClass("NotClient");
    if (client)
    {
        txt.attr("autocomplete","off");
        var sty = "position:absolute;border:1px solid gray;display:none;";
        var out = $("<iframe id='hoogle-output' style='" + sty + "' />");
        out.load(function(){
            out.contents().find("head").html(
                "<style type='text/css'>" +
                "body {font-family: sans-serif; font-size: 13px; background-color: white; padding: 0px; margin: 0px;}" +
                "a, i {display: block; color: black; padding: 2px 6px; text-decoration: none; white-space: nowrap; overflow: hidden;}" +
                "a.sel {background-color: #ccc;}" +
                "</style>");
        });
        out.appendTo(txt.parent());

        txt.keydown(function(event){
            var KeyDown = 40, KeyUp = 38, KeyEnter = 13;
            if (event.which == KeyEnter)
            {
                var sel = out.contents().find(".sel:first");
                if (sel.size() != 0)
                {
                    event.preventDefault();
                    document.location.href = sel.attr("href");
                }
                return;
            }

            var i = event.which == KeyDown ? 1 :
                    event.which == KeyUp ? -1 : 0;
            if (i == 0) return;
            var all = out.contents().find("a");
            var sel = out.contents().find(".sel");
            var now = all.index(sel);
            if (now == -1)
            {
                all.filter(":first").addClass("sel");
            }
            else
            {
                sel.removeClass("sel");
                all.filter(":eq(" + (now+i) + ")").addClass("sel");
            }
            event.preventDefault();
        });
        
        // txt.blur(function(){out.css("display","none");});
    }
    else
        txt.focus();
    currentSearch = txt.keyup(searchBoxChange).val();
});

function searchBoxChange()
{
    if (!client && !instant) return;
    var txt = $("#hoogle");
    var now = txt.val();
    if (now == currentSearch) return; else currentSearch = now;
    var old = oldSearches.ask(now);
    if (old != undefined)
    {
        if (client)
            showClient(old);
        else
            $("#body").html(old);
    }
    else
    {
        if (timeoutId != undefined) window.clearTimeout(timeoutId);
        if (client && now == "") {$("#hoogle-output").css("display","none"); return;}

        timeoutId = window.setTimeout(function(){
            timeoutId = undefined;
            if (client)
                showClient("<i>Still working...</i>");
            else
                $("h1").text("Still working...");
        }, 500);

        $.ajax({
            url: !client ? '?' : $("#hoogle").parent().attr("action") + "?",
            data: {mode:client ? 'embed' : 'ajax', hoogle:now},
            dataType: 'html',
            complete: function(s){return function(e){
                window.clearTimeout(timeoutId);
                timeoutId = undefined;
                if (e.status == 200)
                {
                    oldSearches.add(s,e.responseText);
                    if (txt.val() == s)
                    {
                        if (client)
                            showClient(e.responseText);
                        else
                            $("#body").html(e.responseText);
                    }
                }
                else
                {
                    if (client)
                        showClient("<i>Error: status " + e.status + "</i>");
                    else
                        $("#body").html("<h1><b>Error:</b> status " + e.status + "</h1><p>" + e.responseText + "</p>");
                }
            }}(now)
        });
    }
}

function showClient(x)
{
    $("#hoogle-output").css("display","").contents().find("body").empty().append(x).find("a").attr("target","_parent");
}


/////////////////////////////////////////////////////////////////////
// INSTANT

$(function(){
    if (client) return;
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
    if (client) return;
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
