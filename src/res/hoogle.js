// registered with the body onload event
function on_load()
{
    document.getElementById('q').focus();
}

// add hoogle as a quicksearch tag, available on Firefox/Mozilla
function addHoogle()
{
	addEngine('hoogle','png','Programming','4691');
}

function addEngine(name,ext,cat,pid)
{
	if ((typeof window.sidebar == "object") && (typeof window.sidebar.addSearchEngine == "function"))
	{
		window.sidebar.addSearchEngine(
			'http://mycroft.mozdev.org/install.php/' + pid + "/" + name + ".src",
			'http://mycroft.mozdev.org/install.php/' + pid + "/" + name + "."+ ext, name, cat);
	} else {
		alert("You will need a browser which supports Sherlock to install this plugin.");
	}
}

