
module Web.Page(header, footer, welcome) where

import Web.Text

header query =
    ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>"
    ,"  <head profile='http://a9.com/-/spec/opensearch/1.1/'>"
    ,"     <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1' />"
    ,"     <title>" ++ (query +? " - ") ++ "Hoogle</title>"
    ,"     <link type='text/css' rel='stylesheet' href='res/hoogle.css' />"
    ,"     <link type='image/png' rel='icon' href='res/favicon.png' />"
    ,"     <link type='application/opensearchdescription+xml' rel='search' href='res/search.xml' title='Hoogle' />"
    ,"     <script type='text/javascript' src='res/hoogle.js'> </script>"
    ,"  </head>"
    ,"  <body onload='on_load()'>"
    ] ++ links ++ search query


links =
    ["<div id='links'>"
    ,"  <span id='plugin' style='display:none;'><a href='javascript:add_search()'>Search plugin</a> |</span>"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>Manual</a> |"
    ,"  <a href='http://www.haskell.org/'>haskell.org</a>"
    ,"</div>"
    ]

search query =
    ["<form action='.' method='get'>"
    ,"  <div id='search'>"
    ,"    <a id='logo' href='http://haskell.org/hoogle/'>" ++
           "<img src='res/hoogle.png' alt='Hoogle' />" ++
         "</a>"
    ,"    <input name='hoogle' id='hoogle' type='text' value=\"" ++ query ++ "\" />"
    ,"    <input id='submit' type='submit' value='Search' />"
    ,"  </div>"
    ,"</form>"
    ]


footer =
    ["    <p id='footer'>&copy; <a href='http://www.cs.york.ac.uk/~ndm/'>Neil Mitchell</a> 2004-2008</p>"
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
    ,"  <a href='http://www-users.cs.york.ac.uk/~ndm/contact/'>email me</a>, or add an entry to my"
    ,"  <a href='http://code.google.com/p/ndmitchell/issues/list'>bug tracker</a>."
    ,"</p>"
    ]
    where
        search x = "<a href='?hoogle=" +% x ++ "'>" +& x ++ "</a><br/>"
