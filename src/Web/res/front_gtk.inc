<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
		<title>Hoogle</title>
		<link type="text/css" rel="stylesheet" href="res/hoogle.css" />

		<script type="text/javascript">
function addHoogle()
{
	addEngine('hoogle','png','Programming','4691');
}

function addEngine(name,ext,cat,pid)
{
  if ((typeof window.sidebar == "object") && (typeof window.sidebar.addSearchEngine == "function")) {
    window.sidebar.addSearchEngine(
      "http://mycroft.mozdev.org/install.php/" + pid + "/" + name + ".src",
      "http://mycroft.mozdev.org/install.php/" + pid + "/" + name + "."+ ext, name, cat );
  } else {
    alert("You will need a browser which supports Sherlock to install this plugin.");
  }
}

function on_load()
{
	document.getElementById('txt').focus();
}
		</script>
	</head>
	<body onload="on_load()" id="front">


<table id="header">
	<tr>
		<td style="text-align:left;">
			<a href="http://www.haskell.org/">haskell.org</a>
		</td>
		<td style="text-align:right;">
			<!--[if IE]>
			<div style="display:none;">
			<![endif]-->
			<a href="javascript:addHoogle()">Firefox plugin</a> |
			<!--[if IE]>
			</div>
			<![endif]-->
			<a href="http://www.haskell.org/haskellwiki/Hoogle/Tutorial">Tutorial</a> |
			<a href="http://www.haskell.org/haskellwiki/Hoogle">Manual</a>
		</td>
</table>

<img style="float:left;" src="http://gtk.org/images/gtk-logo-rgb.gif" alt="Gtk" />

<div style="width:100%;margin-top:30px;margin-bottom:30px;text-align:center;">
	<img style="vertical-align:top;" src="res/hoogle_large.png" alt="Hoogle" />
	<sup style="font-family:serif;font-weight:bold;font-size:16pt;">3
		<span style="color:#b00;">[&beta;]</span>
	</sup>
	<br/>
	<i>The Haskell API Search Engine</i> - <a href="http://haskell.org/gtk2hs/">Gtk2Hs</a> edition<br/>
	<form id="input" action="?" method="get"
		style="text-align:center;padding-top:20px;display:block;">
		<div>
			<input type="hidden" name="package" value="gtk" />
			<input name="q" id="txt" type="text" style="width:300px;margin-right:5px;" />
			<input style="padding-left:15px;padding-right:15px;" type="submit" value="Search" />
		</div>
	</form>

	<div style="margin:auto;margin-top:40px;padding:3px;width:300px;border:2px solid #cc0;background-color:#ffc;font-size:10pt;text-align:left;">
		Example searches:<br/>
		&nbsp; <a href="?q=map">map</a><br/>
		&nbsp; <a href="?q=(a%20-%3E%20b)%20-%3E%20[a]%20-%3E%20[b]">(a -&gt; b) -&gt; [a] -&gt; [b]</a><br/>
		&nbsp; <a href="?q=Ord%20a%20%3D%3E%20[a]%20-%3E%20[a]">Ord a =&gt; [a] -&gt; [a]</a>
	</div>
</div>

<p style="text-align:center;font-size:10pt;font-style:italic;margin-bottom:3px;">
	"Roses are red. Violets are blue. <a href="http://www.google.com/">Google</a> rocks. Homage to you."
</p>
<p id="footer" style="margin-top:3px;">
	&copy; <a href="http://www.cs.york.ac.uk/~ndm/">Neil Mitchell</a> 2004-2006
</p>


	</body>
</html>
