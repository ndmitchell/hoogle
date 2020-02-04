Hoogle can be integrated into other projects in several ways.

----------

**This page describes how Hoogle 5 might work, and has not yet been fully implemented.** 

----------

# Embedded Hoogle

Embedded Hoogle lets you include a small interactive Hoogle search box on your web page. As an example:

<form action="https://hoogle.haskell.org" method="get">
  <script type="text/javascript" src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
  <script type="text/javascript" src="https://hoogle.haskell.org/hoogle.js"></script>
  <input type="text"   name="hoogle" id="hoogle" accesskey="1" />
  <input type="submit" value="Search" />
</form>

Try entering searches into the box above, for example "filter". As you type, the search box should perform Hoogle searches, and display the results. Selecting a result will visit the associated documentation. Pressing the Search button will perform the search at the Hoogle website.

To use Embedded Hoogle on your page add:

    <form action="https://hoogle.haskell.org" method="get">
        <script type="text/javascript" src="https://code.jquery.com/jquery-3.3.1.min.js"></script>
        <script type="text/javascript" src="https://hoogle.haskell.org/hoogle.js"></script>
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

    $ curl -sS "https://hoogle.haskell.org?mode=json&hoogle=map&start=1&count=2"
    [
      {
        "url": "https://hackage.haskell.org/package/base/docs/Prelude.html#v:map",
        "module": {
          "url": "https://hackage.haskell.org/package/base/docs/Prelude.html",
          "name": "Prelude"
        },
        "package": {
          "url": "https://hackage.haskell.org/package/base",
          "name": "base"
        },
        "item": "<span class=name><s0>map</s0></span> :: (a -&gt; b) -&gt; [a] -&gt; [b]",
        "type": "",
        "docs": "<a>map</a> <tt>f xs</tt> is the list obtained by applying <tt>f</tt>\nto each element of <tt>xs</tt>, i.e.,\n\n<pre>\nmap f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]\nmap f [x1, x2, ...] == [f x1, f x2, ...]\n</pre>\n"
      },
      {
        "url": "https://hackage.haskell.org/package/base/docs/Data-List.html#v:map",
        "module": {
          "url": "https://hackage.haskell.org/package/base/docs/Data-List.html",
          "name": "Data.List"
        },
        "package": {
          "url": "https://hackage.haskell.org/package/base",
          "name": "base"
        },
        "item": "<span class=name><s0>map</s0></span> :: (a -&gt; b) -&gt; [a] -&gt; [b]",
        "type": "",
        "docs": "<a>map</a> <tt>f xs</tt> is the list obtained by applying <tt>f</tt>\nto each element of <tt>xs</tt>, i.e.,\n\n<pre>\nmap f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]\nmap f [x1, x2, ...] == [f x1, f x2, ...]\n</pre>\n"
      }
    ]

Another possible option is to set the `?format` query parameter to `text` (only current possible value), to remove the HTML tags from the `item` and `docs` properties of the output.

    $ curl -sS "https://hoogle.haskell.org?mode=json&format=text&hoogle=map&start=1&count=2"
    [
      {
        "url": "https://hackage.haskell.org/package/base/docs/Prelude.html#v:map",
        "module": {
          "url": "https://hackage.haskell.org/package/base/docs/Prelude.html",
          "name": "Prelude"
        },
        "package": {
          "url": "https://hackage.haskell.org/package/base",
          "name": "base"
        },
        "item": "map :: (a -> b) -> [a] -> [b]",
        "type": "",
        "docs": "map f xs is the list obtained by applying f\nto each element of xs, i.e.,\n\n\nmap f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]\nmap f [x1, x2, ...] == [f x1, f x2, ...]\n\n"
      },
      {
        "url": "https://hackage.haskell.org/package/base/docs/Data-List.html#v:map",
        "module": {
          "url": "https://hackage.haskell.org/package/base/docs/Data-List.html",
          "name": "Data.List"
        },
        "package": {
          "url": "https://hackage.haskell.org/package/base",
          "name": "base"
        },
        "item": "map :: (a -> b) -> [a] -> [b]",
        "type": "",
        "docs": "map f xs is the list obtained by applying f\nto each element of xs, i.e.,\n\n\nmap f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]\nmap f [x1, x2, ...] == [f x1, f x2, ...]\n\n"
      }
    ]
    
These JSON outputs have been reformatted to better fit the screen. Future versions of Hoogle may produce different JSON output.
