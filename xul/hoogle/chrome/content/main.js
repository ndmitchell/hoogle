
function on_load()
{
    runHoogle("");
}

function trapclick(e)
{
    var url = e.currentTarget.toString();
    var uri = Components
        .classes["@mozilla.org/network/simple-uri;1"]
        .getService(Components.interfaces.nsIURI);
    uri.spec = url;

    if (uri.scheme == "http") {
        Components
            .classes["@mozilla.org/uriloader/external-protocol-service;1"]
            .getService(Components.interfaces.nsIExternalProtocolService)
            .loadUrl(uri)
    }
    else
    {
        var s = url.substr(url.indexOf("="));
        alert(s);
        runHoogle(uri.query);
        if (uri.ref) {
            alert("Handle ref: " + uri.ref);
        }
    }

    return false;
}


function debug(x)
{
    var s = "" + x;
    for (var i in x)
        s += "\n" + i + " = " + x[i];
    alert(s);
}

function beep()
{
    alert("beep");
}


function runHoogle(cmd)
{
    var file = Components
        .classes["@mozilla.org/file/local;1"]
        .createInstance(Components.interfaces.nsILocalFile);
    file.initWithPath("C:\\Program Files\\Haskell\\bin\\hoogle.exe");

    var proc = Components
        .classes["@mozilla.org/process/util;1"]
        .createInstance(Components.interfaces.nsIProcess);
    proc.init(file);

    var argv = ["/web","/output=C:/Neil/hoogle/src/temp.htm","filter"];
    proc.run(true, argv, argv.length);


    var iframe = document.getElementById("iframe");
    iframe.contentDocument.body.className = "";
    iframe.webNavigation.reload(0);
    
    runHoogle_cont();
}

function runHoogle_cont()
{
    var iframe = document.getElementById("iframe");
    if (iframe.contentDocument.body.className != "loaded")
    {
        window.setTimeout(runHoogle_cont, 100);
        return;
    }

    iframe.contentDocument.body.setAttribute("id","xul");
    
    var base = iframe.contentDocument.createElement("base");
    base.setAttribute("href","file:///c:/neil/hoogle/src/");
    var head = iframe.contentDocument.documentElement.firstChild;
    head.insertBefore(base, head.firstChild);
    
    links = iframe.contentDocument.getElementsByTagName("a");
    for (var i in links)
    {
        if (!links[i].onclick)
            links[i].onclick = trapclick;
    }
}
