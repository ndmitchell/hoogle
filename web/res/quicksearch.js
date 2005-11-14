

function quicksearch()
{
	if ((typeof window.sidebar == "object") &&
	    (typeof window.sidebar.addSearchEngine == "function"))
	{
		window.sidebar.addSearchEngine(
			"http://www-users.cs.york.ac.uk/~ndm/hoogle/hoogle.src",
			"http://www-users.cs.york.ac.uk/~ndm/hoogle/hoogle.png",
			"Hoogle",
			"Computer");
	}
	else
	{
		alert("You don't have a Mozilla based browser, sorry");
	}
}
