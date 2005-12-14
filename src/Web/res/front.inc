<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
		<title>Hoogle</title>

		<link type="text/css" rel="stylesheet" href="res/hoogle.css" />

		<script type="text/javascript">
		function on_load()
		{
			document.getElementById('txt').focus();
		}
		</script>
	</head>
	<body onload="on_load()" id="front">


<table style="width:100%;margin-bottom:30px;">
	<tr>
		<td style="text-align:center;padding-top:15px;">
			<img style="vertical-align:top;" src="res/hoogle_large.png" alt="Hoogle" />
			<sup style="font-family:serif;font-weight:bold;font-size:16pt;">3
				<span style="color:#b00;">[&beta;]</span>
			</sup><br/>
			<i>The Haskell API Search Engine</i><br/>
			<form id="input" action="?" method="get"
				style="text-align:center;padding-top:20px;display:block;">
				<div>
					<input name="q" id="txt" type="text" style="width:300px;margin-right:5px;" />
					<input style="padding-left:15px;padding-right:15px;" type="submit" value="Search" />
				</div>
			</form>

			<div id="example">
				Example searches:<br/>
				<a href="?q=map">map</a><br/>
				<a href="?q=(a%20-%3E%20b)%20-%3E%20[a]%20-%3E%20[b]">(a -&gt; b) -&gt; [a] -&gt; [b]</a><br/>
				<a href="?q=Ord%20a%20%3D%3E%20[a]%20-%3E%20[a]">Ord a =&gt; [a] -&gt; [a]</a>
			</div>
		</td>
		<td id="buttons">
			<a href="/" id="haskell">haskell.org</a>
			<a href="help.htm" id="help">Help</a>
			<a href="about.htm" id="about">About</a>
			<a href="download.htm" id="down">Download</a>
			<a href="developers.htm" id="dev">Developers</a>
			<a href="academics.htm" id="acad">Academics</a>
		</td>
	</tr>
</table>

<p id="footer">
	&copy; <a href="http://www.cs.york.ac.uk/~ndm/">Neil Mitchell</a> 2004-2005<br/>
	<i>"Roses are red. Violets are blue. <a href="http://www.google.com/">Google</a> rocks. Homage to you."</i>
</p>


	</body>

</html>
