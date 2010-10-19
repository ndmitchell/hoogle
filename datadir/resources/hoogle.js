
// registered with the body onload event
function on_load()
{
    document.getElementById("hoogle").focus();

    if (document.location.hash != "")
        document.location.hash = document.location.hash;

    if (window.external && ("AddSearchProvider" in window.external))
        document.getElementById("plugin").style.display = "";

    document.body.className = "loaded";
}

function add_search()
{
    window.external.AddSearchProvider("http://haskell.org/hoogle/res/search.xml");
}

function docs(i)
{
    var e = document.getElementById("d" + i);
    e.className = (e.className == "shut" ? "open" : "shut");
    return false;
}
