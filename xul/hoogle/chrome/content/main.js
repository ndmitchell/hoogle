
// The last command that was run as a Hoogle query
var lastCmd;

var hoogle = getHoogle();
var xuldir = getXulDir();
var iframesrc = getIframeSrc();

function getIframeSrc()
{
    var src = xuldir.parent.parent;
    src.append("src");
    src.append("temp.htm");
    return src.path;
}

function on_load()
{
  try {
    if (hoogle)
    {
        runHoogle("");
    }
    else
    {
        alert("Hoogle Local cannot find the Hoogle executable\n\n" +
              "Please ensure Hoogle is installed on your system, and is accessible " +
              "from the PATH environment variable");
        window.close();
    }

  } catch(e) {alert("Error in on_load: " + e);}
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
        s = /\?hoogle=([^\?#]*)/.exec(url)[1];
        s = decodeURIComponent(s.replace(/\+/g," "));
        if (url.indexOf("#more") == -1)
            runHoogle(s);
        else
        {
            // TODO: Should support #more links
            alert("Hoogle Local does not currently support 'more'");
        }
    }

    return false;
}

function trapsubmit(e)
{
    var iframe = document.getElementById("iframe");
    var q = iframe.contentDocument.getElementById("hoogle").value;
    runHoogle(q);
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
  try {
    lastCmd = cmd;

    var proc = Components
        .classes["@mozilla.org/process/util;1"]
        .createInstance(Components.interfaces.nsIProcess);
    proc.init(hoogle);

    var argv = ["/web","/output=" + iframesrc,cmd];
    proc.run(true, argv, argv.length);


    var iframe = document.getElementById("iframe");
    iframe.contentDocument.body.className = "";
    iframe.webNavigation.loadURI("file:///" + iframesrc,0,null,null,null);
    
    runHoogle_cont();
    
  } catch(e) {alert("Error in runHoogle: " + e);}
}

function runHoogle_cont()
{
  try {
    // Check the document has loaded
    var iframe = document.getElementById("iframe");
    // TODO: Could be done better with a nsIWebProgress on the iframe's <browser>
    if (iframe.contentDocument.body.className != "loaded")
    {
        window.setTimeout(runHoogle_cont, 100);
        return;
    }

    // change the document id, triggers various style changes
    iframe.contentDocument.body.setAttribute("id","xul");

    // insert a base element
    // Does not appear to be possible, after document is loaded
    // Best alternative is to repoint all the .js/.css links
    /*
    var base = iframe.contentDocument.createElement("base");
    // TODO: Hard coded path
    base.setAttribute("href","file:///c:/neil/hoogle/src/");
    var head = iframe.contentDocument.documentElement.firstChild;
    head.insertBefore(base, head.firstChild);
    */

    // repoint all the <a> links    
    links = iframe.contentDocument.getElementsByTagName("a");
    for (var i in links)
    {
        if (!links[i].onclick)
            links[i].onclick = trapclick;
    }
    
    // repoint the submit button
    forms = iframe.contentDocument.getElementsByTagName("form");
    for (var i in forms)
        forms[i].onsubmit = trapsubmit;

    // fix the text element
    iframe.contentDocument.getElementById("hoogle").value = lastCmd;

  } catch(e) {alert("Error in runHoogle_cont: " + e);}
}


// Find the Hoogle binary on the PATH
// getHoogle :: nsILocalFile
function getHoogle()
{
  try {
    var windows = Components
        .classes["@mozilla.org/xre/app-info;1"]
        .getService(Components.interfaces.nsIXULRuntime)
        .OS == "WINNT";
    var pathSep = windows ? ";" : ":";
    var exeExt = windows ? ".exe" : "";

    var path = Components
        .classes["@mozilla.org/process/environment;1"]
        .createInstance(Components.interfaces.nsIEnvironment)
        .get("PATH");
    var paths = path.split(pathSep);

    var file = Components
        .classes["@mozilla.org/file/local;1"]
        .createInstance(Components.interfaces.nsILocalFile);

    for (var i in paths)
    {
        file.initWithPath(paths[i]);
        file.append("hoogle" + exeExt);
        if (file.exists())
            return file;
    }
    return undefined;

  } catch(e) {alert("Error in getHoogle: " + e);}
}


// Find the XUL directory
// getXulDir :: nsIFile
function getXulDir()
{
    return Components
        .classes['@mozilla.org/file/directory_service;1']
        .getService(Components.interfaces.nsIProperties)
        .get("resource:app",Components.interfaces.nsIFile);
}
