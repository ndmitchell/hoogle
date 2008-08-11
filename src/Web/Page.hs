
module Web.Page(header, footer) where

import Web.Text

header query =
    ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html>"
    ,"  <head>"
    ,"     <meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1' />"
    ,"     <title>" ++ (query +? " - ") ++ "Hoogle</title>"
    ,"     <link type='text/css' rel='stylesheet' href='res/hoogle.css'>"
    ,"     <link type='image/png' rel='icon' href='res/favicon.png'>"
    ,"     <script type='text/javascript' src='res/hoogle.js'> </script>"
    ,"  </head>"
    ,"  <body onload='on_load()'>"
    ] ++ links ++ search query


links =
    ["<div id='links'>"
    ,"  <a href='http://www.haskell.org/haskellwiki/Hoogle'>Manual</a> |"
    ,"  <a href='http://www.haskell.org/'>haskell.org</a>"
    ,"</div>"
    ]

search query =
    ["<form action='?' method='get' id='search'>"
    ,"  <a id='logo' href='http://haskell.org/hoogle/'>" ++
         "<img src='res/hoogle.png' alt='Hoogle' />" ++
       "</a>"
    ,"  <input name='q' id='q' type='text' value=\"" ++ query ++ "\" />"
    ,"  <input id='submit' type='submit' value='Search' />"
    ,"</form>"
    ]


footer =
    ["    <p id='footer'>&copy; <a href='http://www.cs.york.ac.uk/~ndm/'>Neil Mitchell</a> 2004-2008</p>"
    ,"  </body>"
    ,"</html>"
    ]
