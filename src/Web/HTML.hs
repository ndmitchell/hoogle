{-# LINE 4 "HTML.hsx" #-}
module Web.HTML
       (WebData(..), htmlFront, htmlError, htmlAnswers, innerNoResult)
       where
{-# LINE 6 "HTML.hsx" #-}
import Web.XML
{-# LINE 7 "HTML.hsx" #-}
import Hoogle.General
 
{-# LINE 10 "HTML.hsx" #-}
data WebData = WebData{webSearch :: String, webPackage :: String,
                       webLogo :: String}
{-# LINE 15 "HTML.hsx" #-}
doctype
  = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
{-# LINE 18 "HTML.hsx" #-}
anyPage webData@WebData{webSearch = webSearch} body
  = doctype ++ "\n" ++ show html
  where {-# LINE 20 "HTML.hsx" #-}
        html
          = (genTag (Nothing, "html") []
               [toXMLs
                  ((genTag (Nothing, "head") []
                      [toXMLs
                         ((genETag (Nothing, "meta")
                             [toAttribute ("http-equiv" := "Content-Type"),
                              toAttribute ("content" := "text/html; charset=iso-8859-1")])),
                       toXMLs
                         ((genTag (Nothing, "title") []
                             [toXMLs (if null webSearch then "" else webSearch ++ " - "),
                              toXMLs (pcdata "Hoogle")])),
                       toXMLs
                         ((genETag (Nothing, "link")
                             [toAttribute ("type" := "text/css"),
                              toAttribute ("rel" := "stylesheet"),
                              toAttribute ("href" := "res/hoogle.css")])),
                       toXMLs
                         ((genTag (Nothing, "script")
                             [toAttribute ("type" := "text/javascript"),
                              toAttribute ("src" := "res/hoogle.js")]
                             [toXMLs (pcdata " ")]))])),
                toXMLs
                  ((genTag (Nothing, "body")
                      [toAttribute ("onload" := "on_load()"),
                       toAttribute
                         ("id" := (if null webSearch then "front" else "answers"))]
                      [toXMLs
                         ((genTag (Nothing, "table") [toAttribute ("id" := "header")]
                             [toXMLs
                                ((genTag (Nothing, "tr") []
                                    [toXMLs
                                       ((genTag (Nothing, "td")
                                           [toAttribute ("style" := "text-align:left;")]
                                           [toXMLs
                                              ((genTag (Nothing, "a")
                                                  [toAttribute
                                                     ("href" := "http://www.haskell.org/")]
                                                  [toXMLs (pcdata "haskell.org")]))])),
                                     toXMLs
                                       ((genTag (Nothing, "td")
                                           [toAttribute ("style" := "text-align:right;")]
                                           [toXMLs (rawXml "<!--[if IE]>"),
                                            toXMLs
                                              ((genTag (Nothing, "div")
                                                  [toAttribute ("style" := "display:none;")]
                                                  [toXMLs (rawXml "<![endif]-->"),
                                                   toXMLs
                                                     ((genTag (Nothing, "a")
                                                         [toAttribute
                                                            ("href" := "javascript:addHoogle()")]
                                                         [toXMLs (pcdata "Firefox plugin")])),
                                                   toXMLs (pcdata " "),
                                                   toXMLs (pcdata "|\n                        "),
                                                   toXMLs (rawXml "<!--[if IE]>")])),
                                            toXMLs (rawXml "<![endif]-->"),
                                            toXMLs
                                              ((genTag (Nothing, "a")
                                                  [toAttribute
                                                     ("href" :=
                                                        "http://www.haskell.org/haskellwiki/Hoogle/Tutorial")]
                                                  [toXMLs (pcdata "Tutorial")])),
                                            toXMLs (pcdata " "),
                                            toXMLs (pcdata "|\n                        "),
                                            toXMLs
                                              ((genTag (Nothing, "a")
                                                  [toAttribute
                                                     ("href" :=
                                                        "http://www.haskell.org/haskellwiki/Hoogle")]
                                                  [toXMLs (pcdata "Manual")]))]))]))])),
                       toXMLs (body),
                       toXMLs
                         ((genTag (Nothing, "p") [toAttribute ("id" := "footer")]
                             [toXMLs (pcdata "&copy; "),
                              toXMLs
                                ((genTag (Nothing, "a")
                                    [toAttribute ("href" := "http://www.cs.york.ac.uk/~ndm/")]
                                    [toXMLs (pcdata "Neil Mitchell")])),
                              toXMLs (pcdata " "),
                              toXMLs (pcdata "2004-2006\n            ")]))]))])
 
{-# LINE 59 "HTML.hsx" #-}
searchPage :: (ToXMLs a) => WebData -> a -> String
{-# LINE 60 "HTML.hsx" #-}
searchPage
  webData@WebData{webPackage = webPackage, webLogo = webLogo} inner
  = anyPage webData
      (genTag (Nothing, "hsx") []
         [toXMLs
            ((genTag (Nothing, "div") [toAttribute ("id" := "logo")]
                [toXMLs
                   (iff (not $ null webPackage) $
                      (genETag (Nothing, "img")
                         [toAttribute ("src" := ("res/" ++ webPackage ++ "_small.png")),
                          toAttribute ("alt" := webPackage)])),
                 toXMLs
                   ((genTag (Nothing, "a") [toAttribute ("href" := ".")]
                       [toXMLs
                          ((genETag (Nothing, "img")
                              [toAttribute ("src" := ("res/hoogle_" ++ webLogo ++ "_small.png")),
                               toAttribute ("alt" := "Hoogle")]))]))])),
          toXMLs
            ((genTag (Nothing, "form")
                [toAttribute ("action" := "?"), toAttribute ("method" := "get")]
                [toXMLs
                   ((genTag (Nothing, "div") []
                       [toXMLs
                          (iff (not $ null webPackage)
                             (genETag (Nothing, "input")
                                [toAttribute ("type" := "hidden"),
                                 toAttribute ("name" := "package"),
                                 toAttribute ("value" := webPackage)])),
                        toXMLs
                          (iff (webLogo /= "default")
                             (genETag (Nothing, "input")
                                [toAttribute ("name" := "q"), toAttribute ("type" := "hidden"),
                                 toAttribute ("value" := webLogo)])),
                        toXMLs
                          ((genETag (Nothing, "input")
                              [toAttribute ("name" := "q"), toAttribute ("id" := "txt"),
                               toAttribute ("type" := "text"),
                               toAttribute ("style" := "width:300px;margin-right:5px;"),
                               toAttribute ("value" := (webSearch webData))])),
                        toXMLs
                          ((genETag (Nothing, "input")
                              [toAttribute ("style" := "padding-left:15px;padding-right:15px;"),
                               toAttribute ("type" := "submit"),
                               toAttribute ("value" := "Search")]))]))])),
          toXMLs (inner)])
 
{-# LINE 89 "HTML.hsx" #-}
htmlError :: WebData -> String -> String
{-# LINE 90 "HTML.hsx" #-}
htmlError webData errmsg
  = searchPage webData $
      (genTag (Nothing, "hsx") []
         [toXMLs
            ((genTag (Nothing, "table") [toAttribute ("id" := "heading")]
                [toXMLs
                   ((genTag (Nothing, "tr") []
                       [toXMLs
                          ((genTag (Nothing, "td") [] [toXMLs (pcdata "Invalid Search")])),
                        toXMLs
                          ((genTag (Nothing, "td") [toAttribute ("id" := "count")]
                              [toXMLs (pcdata "No results found")]))]))])),
          toXMLs
            ((genTag (Nothing, "div") [toAttribute ("id" := "failure")]
                [toXMLs (pcdata "Error, your search was invalid:"),
                 toXMLs ((genETag (Nothing, "br") [])), toXMLs (errmsg),
                 toXMLs
                   ((genTag (Nothing, "ul") []
                       [toXMLs
                          ((genTag (Nothing, "li") []
                              [toXMLs
                                 (pcdata
                                    "This is probably a parse error, check for matching brackets etc.")]))]))]))])
 
{-# LINE 110 "HTML.hsx" #-}
innerNoResult :: String
{-# LINE 111 "HTML.hsx" #-}
innerNoResult
  = show $
      (genTag (Nothing, "div") [toAttribute ("id" := "failure")]
         [toXMLs (pcdata "Your search returned no results:\n        "),
          toXMLs
            ((genTag (Nothing, "ul") []
                [toXMLs
                   ((genTag (Nothing, "li") []
                       [toXMLs
                          (pcdata
                             "Make sure you are using the search engine properly, it only searches for Haskell functions")])),
                 toXMLs
                   ((genTag (Nothing, "li") []
                       [toXMLs
                          (pcdata
                             "Try a smaller substring, for example, if you searched for "),
                        toXMLs ((genTag (Nothing, "tt") [] [toXMLs (pcdata "mapConcat")])),
                        toXMLs (pcdata ", try searching for either "),
                        toXMLs ((genTag (Nothing, "tt") [] [toXMLs (pcdata "map")])),
                        toXMLs (pcdata " "), toXMLs (pcdata "or "),
                        toXMLs ((genTag (Nothing, "tt") [] [toXMLs (pcdata "concat")])),
                        toXMLs (pcdata " "), toXMLs (pcdata "individually.")]))]))])
 
{-# LINE 122 "HTML.hsx" #-}
htmlAnswers :: WebData -> String -> String
{-# LINE 123 "HTML.hsx" #-}
htmlAnswers webData inner = searchPage webData inner
 
{-# LINE 130 "HTML.hsx" #-}
htmlFront :: WebData -> String
{-# LINE 131 "HTML.hsx" #-}
htmlFront
  webData@WebData{webPackage = webPackage, webLogo = webLogo}
  = anyPage webData $
      (genTag (Nothing, "div")
         [toAttribute
            ("style" :=
               "width:100%;margin-top:30px;margin-bottom:30px;text-align:center;")]
         [toXMLs
            (iff (not $ null webPackage)
               (genETag (Nothing, "img")
                  [toAttribute ("style" := "vertical-align:top;"),
                   toAttribute ("src" := ("res/" ++ webPackage ++ "_large.png")),
                   toAttribute ("alt" := webPackage)])),
          toXMLs
            ((genETag (Nothing, "img")
                [toAttribute ("style" := "vertical-align:top;"),
                 toAttribute ("src" := ("res/hoogle_" ++ webLogo ++ "_large.png")),
                 toAttribute ("alt" := "Hoogle")])),
          toXMLs
            ((genTag (Nothing, "sup")
                [toAttribute
                   ("style" := "font-family:serif;font-weight:bold;font-size:16pt;")]
                [toXMLs (pcdata "3\n            "),
                 toXMLs
                   ((genTag (Nothing, "span") [toAttribute ("style" := "color:#b00;")]
                       [toXMLs (pcdata "[&beta;]")]))])),
          toXMLs ((genETag (Nothing, "br") [])),
          toXMLs
            ((genTag (Nothing, "i") []
                [toXMLs (pcdata "The Haskell API Search Engine")])),
          toXMLs
            (iff (not $ null webPackage)
               (genTag (Nothing, "hsx") []
                  [toXMLs (pcdata " "), toXMLs (pcdata "- "),
                   toXMLs
                     ((genTag (Nothing, "a")
                         [toAttribute ("href" := "http://haskell.org/gtk2hs/")]
                         [toXMLs (pcdata "Gtk2Hs")])),
                   toXMLs (pcdata " "), toXMLs (pcdata "edition")])),
          toXMLs ((genETag (Nothing, "br") [])),
          toXMLs
            ((genTag (Nothing, "form")
                [toAttribute ("id" := "input"), toAttribute ("action" := ""),
                 toAttribute ("method" := "get"),
                 toAttribute
                   ("style" := "text-align:center;padding-top:20px;display:block;")]
                [toXMLs
                   ((genTag (Nothing, "div") []
                       [toXMLs
                          (iff (not $ null webPackage)
                             (genETag (Nothing, "input")
                                [toAttribute ("type" := "hidden"),
                                 toAttribute ("name" := "package"),
                                 toAttribute ("value" := webPackage)])),
                        toXMLs
                          (iff (webLogo /= "default")
                             (genETag (Nothing, "input")
                                [toAttribute ("name" := "q"), toAttribute ("type" := "hidden"),
                                 toAttribute ("value" := webLogo)])),
                        toXMLs
                          ((genETag (Nothing, "input")
                              [toAttribute ("name" := "q"), toAttribute ("id" := "txt"),
                               toAttribute ("type" := "text"),
                               toAttribute ("style" := "width:300px;margin-right:5px;")])),
                        toXMLs
                          ((genETag (Nothing, "input")
                              [toAttribute ("style" := "padding-left:15px;padding-right:15px;"),
                               toAttribute ("type" := "submit"),
                               toAttribute ("value" := "Search")]))]))])),
          toXMLs
            ((genTag (Nothing, "div")
                [toAttribute
                   ("style" :=
                      "margin:auto;margin-top:40px;padding:3px;width:300px;border:2px solid #cc0;background-color:#ffc;font-size:10pt;text-align:left;")]
                [toXMLs (pcdata "Example searches:"),
                 toXMLs ((genETag (Nothing, "br") [])), toXMLs (pcdata "&nbsp; "),
                 toXMLs
                   ((genTag (Nothing, "a") [toAttribute ("href" := "?q=map")]
                       [toXMLs (pcdata "map")])),
                 toXMLs ((genETag (Nothing, "br") [])), toXMLs (pcdata "&nbsp; "),
                 toXMLs
                   ((genTag (Nothing, "a")
                       [toAttribute
                          ("href" := "?q=(a%20-%3E%20b)%20-%3E%20[a]%20-%3E%20[b]")]
                       [toXMLs (pcdata "(a -&gt; b) -&gt; [a] -&gt; [b]")])),
                 toXMLs ((genETag (Nothing, "br") [])), toXMLs (pcdata "&nbsp; "),
                 toXMLs
                   ((genTag (Nothing, "a")
                       [toAttribute ("href" := "?q=Ord%20a%20%3D%3E%20[a]%20-%3E%20[a]")]
                       [toXMLs (pcdata "Ord a =&gt; [a] -&gt; [a]")]))]))])
