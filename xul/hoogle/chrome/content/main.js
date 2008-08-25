
function on_load()
{
    var iframe = document.getElementById("iframe");
    iframe.contentDocument.body.setAttribute("id","xul");

    links = iframe.contentDocument.getElementsByTagName("a");
    for (var i in links)
    {
        if (!links[i].onclick)
            links[i].onclick = trapclick;
    }
}

function trapclick(e)
{
    var url = e.currentTarget;
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
        runHoogle();
        alert("handle links: " + url);
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



function runHoogle()
{
    var file = Components
        .classes["@mozilla.org/file/local;1"]
        .createInstance(Components.interfaces.nsILocalFile);
    file.initWithPath("C:\\Program Files\\Haskell\\bin\\hoogle.exe");

    var proc = Components
        .classes["@mozilla.org/process/util;1"]
        .createInstance(Components.interfaces.nsIProcess);
    proc.init(file);

    var argv = ["/web","/debug","hoogle_local"];
    proc.run(true, argv, argv.length);
    alert("run");
}
