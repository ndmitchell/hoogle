
module Web.Page(header, footer) where


header query = unlines $
    ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html>"
    ,"  <head>"
    ,"     <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1' />"
    ,"     <title>" ++ query ++ " - Hoogle</title>"
    ,"     <link type='text/css' rel='stylesheet' href='res/hoogle.css'>"
    ,"     <script type='text/javascript' src='res/hoogle.js'> </script>"
    ,"  </head>"
    ,"  <body onload='on_load()'>"
    ] ++ rightBox ++ search


rightBox =
    ["<div id='rightBox'>"
    ,"  <!--[if IE]><span style='display:none;'><![endif]-->"
    ,"    <a href='javascript:addHoogle()'>Firefox plugin</a> |"
    ,"  <!--[if IE]></span><![endif]-->"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>Manual</a> |"
    ,"  <a href='http://www.haskell.org/'>haskell.org</a>"
    ,"</div>"
    ]

search = ["[search]"]


footer = unlines
    ["    <p id='footer'>&copy; <a href='http://www.cs.york.ac.uk/~ndm/'>Neil Mitchell</a> 2004-2008</p>"
    ,"  </body>"
    ,"</html>"
    ]

{-
    
    
    <html><head><title>map - Hoogle</title></link></head><body onload="on_load()" id="answers"><table id="header"><tr><td style="text-align:left;"><a href="http://www.haskell.org/">haskell.org</a></td><td style="text-align:right;"><!--[if IE]><div style="display:none;"><![endif]--><a href="javascript:addHoogle()">Firefox plugin</a> |
                            <!--[if IE]></div><![endif]--><a href="http://www.haskell.org/haskellwiki/Hoogle/Tutorial">Tutorial</a> |
                            <a href="http://www.haskell.org/haskellwiki/Hoogle">Manual</a></td></tr></table><div id="logo"><a href="."><img src="res/hoogle_default_small.png" alt="Hoogle"></img></a></div><form action="?" method="get"><div><input name="q" id="txt" type="text" style="width:300px;margin-right:5px;" value="map"></input><input style="padding-left:15px;padding-right:15px;" type="submit" value="Search"></input></div></form><table id='heading'><tr><td>Searched for <b>map</b></td><td id='count'>Results <b>1</b> - <b>25</b> of <b>129</b> <a href='?start=25&q=map'><img src='res/top_right.png' /></a></td></tr></table><p id='lambdabot'><span class='name'><a href='http://www.cse.unsw.edu.au/~dons/lambdabot.html'>Lambdabot</a> says:</span> <a href='http://www.haskell.org/hawiki/HaskellUserLocations'>http://www.haskell.org/hawiki/HaskellUserLocations</a></p><table id='results'><tr><td class='mod'><a href='hoodoc.cgi?module=Prelude&amp;mode=module'>Prelude</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Prelude&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Prelude&amp;name=map&amp;mode=func'>:: (a -&gt; b) -&gt; [a] -&gt; [b]</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.List&amp;mode=module'>Data.List</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.List&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.List&amp;name=map&amp;mode=func'>:: (a -&gt; b) -&gt; [a] -&gt; [b]</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.ByteString&amp;mode=module'>Data.ByteString</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.ByteString&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.ByteString&amp;name=map&amp;mode=func'>:: (Word8 -&gt; Word8) -&gt; ByteString -&gt; ByteString</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Set&amp;mode=module'>Data.Set</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Set&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Set&amp;name=map&amp;mode=func'>:: (Ord a, Ord b) =&gt; (a -&gt; b) -&gt; Set a -&gt; Set b</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Map&amp;mode=module'>Data.Map</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Map&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Map&amp;name=map&amp;mode=func'>:: (a -&gt; b) -&gt; Map k a -&gt; Map k b</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.IntSet&amp;mode=module'>Data.IntSet</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.IntSet&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.IntSet&amp;name=map&amp;mode=func'>:: (Int -&gt; Int) -&gt; IntSet -&gt; IntSet</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.IntMap&amp;mode=module'>Data.IntMap</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.IntMap&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.IntMap&amp;name=map&amp;mode=func'>:: (a -&gt; b) -&gt; IntMap a -&gt; IntMap b</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.ByteString.Char8&amp;mode=module'>Data.ByteString.Ch..</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.ByteString.Char8&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.ByteString.Char8&amp;name=map&amp;mode=func'>:: (Char -&gt; Char) -&gt; ByteString -&gt; ByteString</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.ByteString.Lazy&amp;mode=module'>Data.ByteString.Lazy</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.ByteString.Lazy&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.ByteString.Lazy&amp;name=map&amp;mode=func'>:: (Word8 -&gt; Word8) -&gt; ByteString -&gt; ByteString</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.ByteString.Lazy.Char8&amp;mode=module'>Data.ByteString.La..</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.ByteString.Lazy.Char8&amp;name=map&amp;mode=func'><b>map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.ByteString.Lazy.Char8&amp;name=map&amp;mode=func'>:: (Char -&gt; Char) -&gt; ByteString -&gt; ByteString</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data&amp;mode=module'>Data</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Map&amp;mode=module'><b>Map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Map&amp;mode=module'>:: <i>module</i></a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Map&amp;mode=module'>Data.Map</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Map&amp;name=Map&amp;mode=data'><b>Map</b></a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Map&amp;name=Map&amp;mode=data'>:: <i>data</i> Map k a</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Prelude&amp;mode=module'>Prelude</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Prelude&amp;name=mapM&amp;mode=func'><b>map</b>M</a></td><td class='typ'><a href='hoodoc.cgi?module=Prelude&amp;name=mapM&amp;mode=func'>:: Monad m =&gt; (a -&gt; m b) -&gt; [a] -&gt; m [b]</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Prelude&amp;mode=module'>Prelude</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Prelude&amp;name=mapM%5f&amp;mode=func'><b>map</b>M_</a></td><td class='typ'><a href='hoodoc.cgi?module=Prelude&amp;name=mapM%5f&amp;mode=func'>:: Monad m =&gt; (a -&gt; m b) -&gt; [a] -&gt; m ()</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Maybe&amp;mode=module'>Data.Maybe</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Maybe&amp;name=mapMaybe&amp;mode=func'><b>map</b>Maybe</a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Maybe&amp;name=mapMaybe&amp;mode=func'>:: (a -&gt; Maybe b) -&gt; [a] -&gt; [b]</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Control.Monad&amp;mode=module'>Control.Monad</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Control.Monad&amp;name=mapM&amp;mode=func'><b>map</b>M</a></td><td class='typ'><a href='hoodoc.cgi?module=Control.Monad&amp;name=mapM&amp;mode=func'>:: Monad m =&gt; (a -&gt; m b) -&gt; [a] -&gt; m [b]</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Control.Monad&amp;mode=module'>Control.Monad</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Control.Monad&amp;name=mapM%5f&amp;mode=func'><b>map</b>M_</a></td><td class='typ'><a href='hoodoc.cgi?module=Control.Monad&amp;name=mapM%5f&amp;mode=func'>:: Monad m =&gt; (a -&gt; m b) -&gt; [a] -&gt; m ()</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Control.Monad&amp;mode=module'>Control.Monad</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Control.Monad&amp;name=mapAndUnzipM&amp;mode=func'><b>map</b>AndUnzipM</a></td><td class='typ'><a href='hoodoc.cgi?module=Control.Monad&amp;name=mapAndUnzipM&amp;mode=func'>:: Monad m =&gt; (a -&gt; m (b, c)) -&gt; [a] -&gt; m ([b], [c])</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.List&amp;mode=module'>Data.List</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.List&amp;name=mapAccumL&amp;mode=func'><b>map</b>AccumL</a></td><td class='typ'><a href='hoodoc.cgi?module=Data.List&amp;name=mapAccumL&amp;mode=func'>:: (acc -&gt; x -&gt; (acc, y)) -&gt; acc -&gt; [x] -&gt; (acc, [y])</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.List&amp;mode=module'>Data.List</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.List&amp;name=mapAccumR&amp;mode=func'><b>map</b>AccumR</a></td><td class='typ'><a href='hoodoc.cgi?module=Data.List&amp;name=mapAccumR&amp;mode=func'>:: (acc -&gt; x -&gt; (acc, y)) -&gt; acc -&gt; [x] -&gt; (acc, [y])</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Monoid&amp;mode=module'>Data.Monoid</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Monoid&amp;name=mappend&amp;mode=func'><b>map</b>pend</a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Monoid&amp;name=mappend&amp;mode=func'>:: Monoid a =&gt; a -&gt; a -&gt; a</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Control.Exception&amp;mode=module'>Control.Exception</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Control.Exception&amp;name=mapException&amp;mode=func'><b>map</b>Exception</a></td><td class='typ'><a href='hoodoc.cgi?module=Control.Exception&amp;name=mapException&amp;mode=func'>:: (Exception -&gt; Exception) -&gt; a -&gt; a</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Foldable&amp;mode=module'>Data.Foldable</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Foldable&amp;name=mapM%5f&amp;mode=func'><b>map</b>M_</a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Foldable&amp;name=mapM%5f&amp;mode=func'>:: (Foldable t, Monad m) =&gt; (a -&gt; m b) -&gt; t a -&gt; m ()</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Data.Traversable&amp;mode=module'>Data.Traversable</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Data.Traversable&amp;name=mapM&amp;mode=func'><b>map</b>M</a></td><td class='typ'><a href='hoodoc.cgi?module=Data.Traversable&amp;name=mapM&amp;mode=func'>:: (Traversable t, Monad m) =&gt; (a -&gt; m b) -&gt; t a -&gt; m (t b)</a></td></tr>
    <tr><td class='mod'><a href='hoodoc.cgi?module=Distribution.Configuration&amp;mode=module'>Distribution.Confi..</a>.</td><td class='fun'><a href='hoodoc.cgi?module=Distribution.Configuration&amp;name=mapTreeData&amp;mode=func'><b>map</b>TreeData</a></td><td class='typ'><a href='hoodoc.cgi?module=Distribution.Configuration&amp;name=mapTreeData&amp;mode=func'>:: (a -&gt; b) -&gt; CondTree v c a -&gt; CondTree v c b</a></td></tr>
    </table><div id='select'> <a class='active' href='?start=0&q=map'>1</a>  <a href='?start=25&q=map'>2</a>  <a href='?start=50&q=map'>3</a>  <a href='?start=75&q=map'>4</a>  <a href='?start=100&q=map'>5</a>  <a href='?start=125&q=map'>6</a>  <a href='?start=25&q=map'><img src='res/bot_right.png' /></a></div><p id="footer">&copy; <a href="http://www.cs.york.ac.uk/~ndm/">Neil Mitchell</a> 2004-2008
                </p></body></html>

-}
