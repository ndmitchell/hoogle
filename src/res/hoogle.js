// registered with the body onload event
function on_load()
{
    document.getElementById("hoogle").focus();

    if (document.location.hash != "")
        document.location.hash = document.location.hash;

    if (window.external.AddSearchProvider)
        document.getElementById("plugin").style.display = "";
}

function add_search()
{
    window.external.AddSearchProvider("http://haskell.org/hoogle/res/search.xml");
}

function doc_more(i)
{
    document.getElementById("s" + i).style.display = "none";
    document.getElementById("l" + i).style.display = "block";
    return false;
}

function doc_less(i)
{
    document.getElementById("l" + i).style.display = "none";
    document.getElementById("s" + i).style.display = "block";
    return false;
}
