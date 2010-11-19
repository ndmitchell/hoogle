
module Web.Page(header, footer, welcome) where

import General.Web
import General.Util


header resources query =
    ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>"
    ,"  <head profile='http://a9.com/-/spec/opensearch/1.1/'>"
    ,"     <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1' />"
    ,"     <title>" ++ (query ++? " - ") ++ "Hoogle</title>"
    ,"     <link type='text/css' rel='stylesheet' href='" ++ resources ++ "/hoogle.css' />"
    ,"     <link type='image/png' rel='icon' href='" ++ resources ++ "/favicon.png' />"
    ,"     <link type='application/opensearchdescription+xml' rel='search' href='" ++ resources ++ "/search.xml' title='Hoogle' />"
    ,"     <script type='text/javascript' src='" ++ resources ++ "/jquery-1.4.2.js'> </script>"
    ,"     <script type='text/javascript' src='" ++ resources ++ "/hoogle.js'> </script>"
    ,"  </head>"
    ,"  <body>"
    ] ++ links ++ search resources query ++
    ["<div id='body'>"]


links =
    ["<div id='links'>"
    ,"  <span id='plugin' style='display:none;'><a href='javascript:searchPlugin()'>Search plugin</a> |</span>"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>Manual</a> |"
    ,"  <a href='http://www.haskell.org/'>haskell.org</a>"
    ,"</div>"
    ]

search resources query =
    ["<form action='.' method='get'>"
    ,"  <div id='search'>"
    ,"    <a id='logo' href='http://haskell.org/hoogle/'>" ++
           "<img src='" ++ resources ++ "/hoogle.png' alt='Hoogle' />" ++
         "</a>"
    ,"    <input name='hoogle' id='hoogle' type='text' autocomplete='false' value=\"" ++ query ++ "\" />"
    ,"    <input id='submit' type='submit' value='Search' />"
    ,"  </div>"
    ,"</form>"
    ]


footer =
    ["</div>"
    ,"    <p id='footer'>&copy; <a href='http://community.haskell.org/~ndm/'>Neil Mitchell</a> 2004-2010</p>"
    ,"  </body>"
    ,"</html>"
    ]


welcome =
    ["<h1><b>Welcome to Hoogle</b></h1>"
    ,"<p>"
    ,"  Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries"
    ,"  by either function name, or by approximate type signature."
    ,"</p>"
    ,"<p id='example'>"
    ,"  Example searches:<br/>"
    ,"  " ++ search "map"
    ,"  " ++ search "(a -> b) -> [a] -> [b]"
    ,"  " ++ search "Ord a => [a] -> [a]"
    ,"  " ++ search "Data.Map.insert"
    ,"  <br/>Enter your own search at the top of the page."
    ,"</p>"
    ,"<p>"
    ,"  The <a href='http://www.haskell.org/haskellwiki/Hoogle'>Hoogle manual</a> contains more details,"
    ,"  including further details on search queries, how to install Hoogle as a command line application"
    ,"  and how to integrate Hoogle with Firefox/Emacs/Vim etc."
    ,"</p>"
    ,"<p>"
    ,"  I am very interested in any feedback you may have. Please "
    ,"  <a href='http://community.haskell.org/~ndm/contact/'>email me</a>, or add an entry to my"
    ,"  <a href='http://code.google.com/p/ndmitchell/issues/list'>bug tracker</a>."
    ,"</p>"
    ]
    where
        search x = "<a href='?hoogle=" ++% x ++ "'>" ++& x ++ "</a><br/>"
