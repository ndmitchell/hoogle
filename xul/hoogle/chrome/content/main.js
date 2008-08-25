
function on_load()
{
    i = document.getElementById("iframe");
    i.contentDocument.body.setAttribute("id","xul");
    /*
    var file = Components.classes["@mozilla.org/file/local;1"].createInstance(Components.interfaces.nsILocalFile); 
    file.initWithPath("start"); 

    alert("here3");
    var process = Components.classes["@mozilla.org/process/util;1"].createInstance(Components.interfaces.nsIProcess); 
    process.init(file); 

    var argv = ["/min","/wait", "hoogle","/web","/debug"];
    alert("here3");
    process.run(true, argv, argv.length);
    */
}
