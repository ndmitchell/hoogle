Hoogle can be integrated into other projects in several ways.

# Embedded Hoogle

Embedded Hoogle lets you include a small interactive Hoogle search box on your web page. As an example:

<form action="http://www.haskell.org/hoogle/" method="get">
  <script type="text/javascript" src="http://www.haskell.org/hoogle/res/jquery.js"></script>
  <script type="text/javascript" src="http://www.haskell.org/hoogle/res/hoogle.js"></script>
  <input type="text"   name="hoogle" id="hoogle" accesskey="1" />
  <input type="submit" value="Search" />
</form>

Try entering searches into the box above, for example "filter". As you type, the search box should perform Hoogle searches, and display the results. Selecting a result will visit the associated documentation. Pressing the Search button will perform the search at the Hoogle website.

To use Embedded Hoogle on your page add:

    <form action="http://www.haskell.org/hoogle/" method="get">
        <script type="text/javascript" src="http://www.haskell.org/hoogle/res/jquery.js"></script>
        <script type="text/javascript" src="http://www.haskell.org/hoogle/res/hoogle.js"></script>
        <input type="text"   name="hoogle" id="hoogle" accesskey="1" />
        <input type="submit" value="Search" />
    </form>

Embedded Hoogle degrades gracefully without Javascript, and works on all common browsers. If you are using IE 7 or below you may not see results unless the page being displayed is on the same server as the Hoogle instance (i.e. haskell.org), due to restrictions on cross domain AJAX requests.

To use a different Hoogle server change the action field of the form. To specify a prefix/suffix for all searches add an input field with the name prefix/suffix. For example, to search only the base package add:

    <input type="hidden" name="prefix" value="+base" />


# Haskell API

Hoogle is available as a standard Haskell library, whose documentation is available at http://hackage.haskell.org/packages/archive/hoogle/latest/doc/html/Hoogle.html. The web version and command-line tools are both built on the Hoogle library.


# JSON API

The Hoogle website provides JSON output using the parameter `?mode=json`. As an example:

    $ wget http://www.haskell.org/hoogle/?mode=json&hoogle=map&start=1&count=2
    {"version":"4.2.11"
    ,"results":
         [{"location":"http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:map"
          ,"self":"map :: (a -> b) -> [a] -> [b]"
          ,"docs":"map f xs is the list obtained by applying f to each element of xs, i.e.,\n\n> map ..."}
         ,{"location":"http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:mapM"
          ,"self":"mapM :: Monad m => (a -> m b) -> [a] -> m [b]"
          ,docs:"mapM f is equivalent to sequence . map f. "}
         ]
     }

This output has been reformatted to better fit the screen. Future versions of Hoogle may produce different JSON output.
