var embed = false; // are we running as an embedded search box
var instant = false; // should we search on key presses
var query = parseQuery(); // what is the current query string

var $hoogle; // $("#hoogle") after load


/////////////////////////////////////////////////////////////////////
// SEARCHING

$(function(){
    $hoogle = $("#hoogle");
    embed = !$hoogle.hasClass("HOOGLE_REAL");
    var self = embed ? newEmbed() : newReal();
    var $form = $hoogle.parents("form:first");

    var ajaxUrl = !embed ? "?" : $form.attr("action") + "?";
    var ajaxMode = embed ? 'embed' : 'ajax';
    var ajaxPrefix = $form.find("input[name=prefix]").attr("value");
    var ajaxSuffix = $form.find("input[name=suffix]").attr("value");

    var active = $hoogle.val(); // What is currently being searched for (may not yet be displayed)
    var past = cache(100); // Cache of previous searches
    var watch = watchdog(500, function(){self.showWaiting();}); // Timeout of the "Waiting..." callback

    $hoogle.keyup(function(){
        if (!instant) return;

        var now = $hoogle.val();
        if (now == active) return;
        active = now;

        var title = now + (now == "" ? "" : " - ") + "Hoogle";
        query["hoogle"] = now;
        if (!embed){
            if (window.history)
                window.history.replaceState(null, title, renderQuery(query));
            $("title").text(title);
        }

        var old = past.ask(now);
        if (old != undefined){self.showResult(old); return;}

        watch.stop();
        if (embed && now == ""){self.hide(); return;}
        watch.start();

        var data = {hoogle:now, mode:ajaxMode, prefix:ajaxPrefix, suffix:ajaxSuffix};
        function complete(e)
        {
            watch.stop();
            if (e.status == 200)
            {
                past.add(now,e.responseText);
                if ($hoogle.val() == now)
                    self.showResult(e.responseText);
            }
            else
                self.showError(e.status, e.responseText);
        }

        var args = {url:ajaxUrl, data:data, complete:complete, dataType:"html"}
        try {
            $.ajax(args);
        } catch (err) {
            try {
                if (!embed) throw err;
                $.ajaxCrossDomain(args);
            } catch (err) {
                // Probably a permissions error from cross domain scripting...
                watch.stop();
            }
        }
    });
})

function newReal()
{
    $hoogle.focus();
    $hoogle.select();
    var $body = $("#body");

    return {
        showWaiting: function(){$("h1").text("Still working...");},
        showError: function(status,text){$body.html("<h1><b>Error:</b> status " + status + "</h1><p>" + text + "</p>")},
        showResult: function(text){$body.html(text); newDocs();}
    }
}

function newEmbed()
{
    $hoogle.attr("autocomplete","off");
    // IE note: unless the div in the iframe contain any border it doesn't calculate the correct outerHeight()
    //          therefore we put 3 borders on the iframe, and leave one for the bottom div
    var $iframe = $("<iframe id='hoogle-output' scrolling='no' "+
                    "style='position:absolute;border:1px solid rgb(127,157,185);border-bottom:0px;display:none;' />");
    var $body;
    $iframe.load(function(){
        var $contents = $iframe.contents();
        $contents.find("head").html(
            "<style type='text/css'>" +
            "html {border: 0px;}" +
            "body {font-family: sans-serif; font-size: 13px; background-color: white; padding: 0px; margin: 0px;}" +
            "a, i {display: block; color: black; padding: 1px 3px; text-decoration: none; white-space: nowrap; overflow: hidden; cursor: default;}" +
            "a.sel {background-color: rgb(10,36,106); color: white;}" +
            "div {border-bottom:1px solid rgb(127,157,185);}" +
            "</style>");
        $body = $("<div>").appendTo($contents.find("body"));
    });
    $iframe.insertBefore($hoogle);

    var finishOnBlur = true; // Should a blur hide the box

    function show(x){
        if (x == undefined)
            $iframe.css("display","none");
        else {
            $body.html(x).find("a").attr("target","_parent")
                .mousedown(function(){finishOnBlur = false;})
                .mouseup(function(){finishOnBlur = true;})
                .mouseenter(function(){
                    $body.find(".sel").removeClass("sel");
                    $(this).addClass("sel");
                });

            var pos = $hoogle.position();
            // need to display before using $body.outerHeight() on Firefox
            $iframe.css("display","").css(
                {top:px(pos.top + $hoogle.outerHeight() + unpx($hoogle.css("margin-top")))
                ,left:px(pos.left + unpx($hoogle.css("margin-left")))
                ,width:px($hoogle.outerWidth() - 2 /* iframe border */)
                ,height:$body.outerHeight()
                });
        }
    }

    $hoogle.blur(function(){if (finishOnBlur) show();});

    $hoogle.keydown(function(event){
        switch(event.which)
        {
        case Key.Return:
            var sel = $body.find(".sel:first");
            if (sel.size() == 0) return;
            event.preventDefault();
            document.location.href = sel.attr("href");
            break;

        case Key.Escape:
            $body.find(".sel").removeClass("sel");
            show();
            break;

        case Key.Down: case Key.Up:
            var i = event.which == Key.Down ? 1 : -1;
            var all = $body.find("a");
            var sel = all.filter(".sel");
            var now = all.index(sel);
            if (now == -1)
                all.filter(i == 1 ? ":first" : ":last").addClass("sel");
            else {
                sel.removeClass("sel");
                // IE treats :eq(-1) as :eq(0), so filter specifically
                if (now+i >= 0) all.filter(":eq(" + (now+i) + ")").addClass("sel");
            }
            event.preventDefault();
            break;
        }
    });

    return {
        showWaiting: function(){show("<i>Still working...</i>");},
        showError: function(status,text){show("<i>Error: status " + status + "</i>");},
        showResult: function(text){show(text);},
        hide: function(){show();}
    }
}


/////////////////////////////////////////////////////////////////////
// INSTANT BUTTON

$(function(){
    if (embed)
        instant = true;
    else
    {
        setInstant(query["ajax"] == "1" || $.cookie("instant") == "1");
        $("#instant").css("display","");
    }
});

function setInstant(x)
{
    instant = x == undefined ? !instant : x ? true : false;
    $("#instantVal").html(instant ? "on" : "off");
    if (instant)
    {
        $.cookie("instant","1",{expires:365});
        $hoogle.keyup();
    }
    else
        $.cookie("instant",null);
}


/////////////////////////////////////////////////////////////////////
// SEARCH PLUGIN

var prefixUrl = document.location.protocol + "//" + document.location.hostname + document.location.pathname;

$(function(){
    if (embed) return;
    if (prefixUrl != "http://haskell.org/hoogle/")
    {
        $("link[rel=search]").attr("href", function(){
            return this.href + "?domain=" + escape(prefixUrl);
        });
    }
    if (window.external && ("AddSearchProvider" in window.external))
        $("#plugin").css("display","");
});

function searchPlugin()
{
    var url = $("link[rel=search]").attr("href");
    if (url.substring(0, prefixUrl.length) != prefixUrl)
        url = prefixUrl + url;
    window.external.AddSearchProvider(url);
}


/////////////////////////////////////////////////////////////////////
// DOCUMENTATION

$(function(){
    if (embed) return;
    $(window).resize(resizeDocs);
    newDocs();
});

function resizeDocs()
{
    $("#body .doc").each(function(){
        // If a segment is open, it should remain open forever
        var $this = $(this);
        var toosmall = ($.support.preWrap && $this.hasClass("newline")) ||
                       ($this.height() < $this.children().height());
        if (toosmall && !$this.hasClass("open"))
            $this.addClass("shut");
        else if (!toosmall && $this.hasClass("shut"))
            $this.removeClass("shut");
    });
}

function newDocs()
{
    resizeDocs();
    $("#body .doc").click(function(){
        var $this = $(this);
        if ($this.hasClass("open") || $this.hasClass("shut"))
            $this.toggleClass("open").toggleClass("shut");
    });
}


/////////////////////////////////////////////////////////////////////
// iOS TWEAKS

$(function(){
    if ($.support.inputSearch)
        $("#hoogle")[0].type = "search";

    var qphone = query["phone"];
    phone =
        qphone == "0" ? false :
        qphone == "1" ? true :
        $.support.phone;

    if (!phone) return;
    $("body").addClass("phone");
    $("head").append("<meta name='viewport' content='width=device-width' />");
});


/////////////////////////////////////////////////////////////////////
// LIBRARY BITS

function parseQuery() // :: IO (Dict String String)
{
    // From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
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

function renderQuery(query) // Dict String String -> IO String
{
    var s = "";
    for (var i in query)
    {
        if (query[i] != "")
            s += (s == "" ? "?" : "&") + i + "=" + encodeURIComponent(query[i]);
    }
    return window.location.href.substring(0, window.location.href.length - window.location.search.length) + s;
}


// Supports white-space: pre-wrap;
$.support.preWrap = !($.browser.msie && $.browser.version < 8);

$.support.iOS =
    (navigator.userAgent.indexOf("iPhone") != -1) ||
    (navigator.userAgent.indexOf("iPod") != -1) ||
    (navigator.userAgent.indexOf("iPad") != -1);

$.support.phone =
    (navigator.userAgent.indexOf("iPhone") != -1) ||
    (navigator.userAgent.indexOf("iPod") != -1) ||
    (navigator.userAgent.indexOf("Android") != -1);

// Supports <input type=search />
$.support.inputSearch = $.support.iOS;

var Key = {
    Up: 38,
    Down: 40,
    Return: 13,
    Escape: 27
};

function unpx(x){var r = 1 * x.replace("px",""); return isNaN(r) ? 0 : r;}
function px(x){return x + "px";}

function cache(maxElems)
{
    // FIXME: Currently does not evict things
    var contents = {}; // what we have in the cache, with # prepended
    // note that contents[toString] != undefined, since it's a default method
    // hence the leading #

    return {
        add: function(key,val)
        {
            contents["#" + key] = val;
        },

        ask: function(key)
        {
            return contents["#" + key];
        }
    };
}

function watchdog(time, fun)
{
    var id = undefined;
    function stop(){if (id == undefined) return; window.clearTimeout(id); id = undefined;}
    function start(){stop(); id = window.setTimeout(function(){id = undefined; fun();}, time);}
    return {start:start, stop:stop}
}

$.ajaxCrossDomain = function(args)
{
    if (!window.XDomainRequest) throw new Error("the XDomainRequest object is not supported in this browser");

    var xdr = new XDomainRequest();
    xdr.onload = function(){args.complete({status:200, responseText:xdr.responseText});};
    xdr.onerror = function(){args.complete({status:0, responseText:""});};

    var url = "";
    for (var i in args.data)
    {
        if (args.data[i] == undefined) continue;
        url += (url == "" ? "" : "&") + encodeURIComponent(i) + "=" + encodeURIComponent(args.data[i]);
    }
    xdr.open("get", args.url + url);
    xdr.send();
}

function on_arrow_press(ev) {
    var offset = 0;
    if (ev.keyCode == Key.Up) {
        offset = -1;
    } else if (ev.keyCode == Key.Down) {
        offset = +1;
    } else if (ev.keyCode != Key.Return) {
        return;
    }

    // Figure out where we are
    var results = $("div#body .result");
    var activeResults = $("div#body .result.active");
    var activeRow = -1;
    if (activeResults.length == 1) {
        activeRow = results.index(activeResults[0]);
    }

    if (ev.keyCode == Key.Return) {
        if (activeRow >= 0)
            document.location.href = $("a", activeResults).attr("href");
    } else {
        var newRow = activeRow + offset;
        var $activeRow = $(results[activeRow]);
        if (newRow < 0) {
            $activeRow.removeClass("active");
            $hoogle.focus();
        } else if (newRow < results.length) {
            var $newRow = $(results[newRow]);
            if (activeRow >= 0)
                $activeRow.removeClass("active");
            $newRow.addClass("active");
            $hoogle.blur();
        }
    }
}

$(document).ready(function() {
    $(document).keyup(on_arrow_press);
});
