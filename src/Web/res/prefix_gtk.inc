<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
		<title>$ - Hoogle</title>
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
	<body onload="on_load()" id="answers">

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

<div id="logo" style="vertical-align: middle">
	<a href=".">
		<img src="res/hoogle_small.png" alt="Hoogle" />
	</a>
	<a href="http://www.haskell.org/gtk2hs/">Gtk2Hs:</a> 
</div>

<form action="?" method="get">
	<div>
		<input type="hidden" name="package" value="gtk" />
		<input name="q" id="txt" type="text" style="width:300px;margin-right:5px;" value="$" />
		<input style="padding-left:15px;padding-right:15px;" type="submit" value="Search" />
	</div>
</form>

