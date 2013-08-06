-- AUTO GENERATED - do not modify
module Web.Page(Templates(..), defaultTemplates, loadTemplates) where
import Web.Template

data Templates = Templates
  {header :: String -> String -> String -> String -> String
  ,footer :: String -> String
  ,welcome :: String
  ,parseError :: String -> String -> String
  }

defaultTemplates :: Templates
defaultTemplates = Templates _header _footer _welcome _parseError

loadTemplates :: String -> Templates
loadTemplates x = Templates _header _footer _welcome _parseError
    where
        [__header,__footer,__welcome,__parseError] = reload x $
            ("header",["css","js","query","queryHyphen"]) :
            ("footer",["version"]) :
            ("welcome",[]) :
            ("parseError",["errFormat","errMessage"]) :
            []
        _header css js query queryHyphen = __header [css,js,query,queryHyphen]
        _footer version = __footer [version]
        _welcome = __welcome []
        _parseError errFormat errMessage = __parseError [errFormat,errMessage]

_header css js query queryHyphen = ""
  ++ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n    <head profile=\"http://a9.com/-/spec/opensearch/1.1/\">\n        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />\n        <title>"
  ++ escapeHTML query
  ++ " "
  ++ escapeHTML queryHyphen
  ++ " Hoogle</title>\n        <link type=\"text/css\" rel=\"stylesheet\" href=\"res/hoogle.css?version="
  ++ escapeURL css
  ++ "\" />\n        <link type=\"image/png\" rel=\"icon\" href=\"res/favicon.png\" />\n\t\t<link type=\"image/png\" rel=\"apple-touch-icon\" href=\"res/favicon57.png\" />\n        <link type=\"application/opensearchdescription+xml\" rel=\"search\" href=\"res/search.xml\" title=\"Hoogle\" />\n        <script type=\"text/javascript\" src=\"res/jquery.js?version="
  ++ escapeURL js
  ++ "\"> </script>\n        <script type=\"text/javascript\" src=\"res/jquery-cookie.js?version="
  ++ escapeURL js
  ++ "\"> </script>\n        <script type=\"text/javascript\" src=\"res/hoogle.js?version="
  ++ escapeURL js
  ++ "\"> </script>\n    </head>\n    <body>\n<div id=\"links\">\n    <ul id=\"top-menu\">\n        <li id=\"instant\" style=\"display:none;\">\n            <a href=\"javascript:setInstant()\">Instant is <span id=\"instantVal\">off</span></a>\n        </li>\n        <li id=\"plugin\" style=\"display:none;\"><a href=\"javascript:searchPlugin()\">Search plugin</a></li>\n        <li><a href=\"https://github.com/ndmitchell/hoogle/blob/master/README.md\">Manual</a></li>\n        <li><a href=\"http://www.haskell.org/\">haskell.org</a>\n</li>\n    </ul>\n</div>\n<form action=\".\" method=\"get\" id=\"search\">\n    <a id=\"logo\" href=\"http://haskell.org/hoogle/\">\n        <img src=\"res/hoogle.png\" width=\"160\" height=\"58\" alt=\"Hoogle\"\n    /></a>\n    <input name=\"hoogle\" id=\"hoogle\" class=\"HOOGLE_REAL\" type=\"text\" autocomplete=\"off\" accesskey=\"1\" value=\""
  ++ escapeHTML query
  ++ "\" />\n    <input id=\"submit\" type=\"submit\" value=\"Search\" />\n</form>\n<div id=\"body\">\n"

_footer version = ""
  ++ "            <div class=\"push\"></div>\n        </div>\n        <div id=\"footer\">&copy; <a href=\"http://community.haskell.org/~ndm/\">Neil Mitchell</a> 2004-2013, version "
  ++ escapeHTML version
  ++ "</div>\n    </body>\n</html>\n"

_welcome = ""
  ++ "<h1><b>Welcome to Hoogle</b></h1>\n<ul id=\"left\">\n<li><b>Links</b></li>\n<li><a href=\"http://haskell.org/\">Haskell.org</a></li>\n<li><a href=\"http://hackage.haskell.org/\">Hackage</a></li>\n<li><a href=\"http://www.haskell.org/ghc/docs/latest/html/users_guide/\">GHC Manual</a></li>\n<li><a href=\"http://www.haskell.org/ghc/docs/latest/html/libraries/\">Libraries</a></li>\n</ul>\n<p>\n    Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries\n    by either function name, or by approximate type signature.\n</p>\n<p id=\"example\">\n    Example searches:<br/>\n     <a href=\"?hoogle=map\">map</a>\n<br/>\n     <a href=\"?hoogle=%28a+-%3e+b%29+-%3e+%5ba%5d+-%3e+%5bb%5d\">(a -&gt; b) -&gt; [a] -&gt; [b]</a>\n<br/>\n     <a href=\"?hoogle=Ord+a+%3d%3e+%5ba%5d+-%3e+%5ba%5d\">Ord a =&gt; [a] -&gt; [a]</a>\n<br/>\n     <a href=\"?hoogle=Data%2eMap%2einsert\">Data.Map.insert</a>\n<br/>\n\t<br/>Enter your own search at the top of the page.\n</p>\n<p>\n    The <a href=\"http://www.haskell.org/haskellwiki/Hoogle\">Hoogle manual</a> contains more details,\n    including further details on search queries, how to install Hoogle as a command line application\n    and how to integrate Hoogle with Firefox/Emacs/Vim etc.\n</p>\n<p>\n    I am very interested in any feedback you may have. Please\n    <a href=\"http://community.haskell.org/~ndm/contact/\">email me</a>, or add an entry to my\n    <a href=\"http://code.google.com/p/ndmitchell/issues/list\">bug tracker</a>.\n</p>\n"

_parseError errFormat errMessage = ""
  ++ "<h1>"
  ++ errFormat
  ++ "</h1>\n<p>\n\t<b>Parse error:</b> "
  ++ escapeHTML errMessage
  ++ "\n</p><p>\n\tFor information on what queries should look like, see the\n\t<a href=\"http://www.haskell.org/haskellwiki/Hoogle\">user manual</a>.\n</p>\n"
