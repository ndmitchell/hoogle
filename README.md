# Hoogle [![Hackage version](https://img.shields.io/hackage/v/hoogle.svg?style=flat)](https://hackage.haskell.org/package/hoogle) [![Build Status](https://img.shields.io/travis/ndmitchell/hoogle.svg?style=flat)](https://travis-ci.org/ndmitchell/hoogle)

**NOTE: This code branch contains the code for the Hoogle v4 series. The `master` branch contains the code for the current Hoogle v5 series.**

Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries by either function name, or by approximate type signature. To experiment, visit the online version at http://haskell.org/hoogle.

* **Online version:** http://haskell.org/hoogle
* **Hackage page:** http://hackage.haskell.org/packages/hoogle
* **Source code:** http://github.com/ndmitchell/hoogle
* **Bug tracker:** http://code.google.com/p/ndmitchell/issues/list

## Hoogle Use

Hoogle can be used in several ways:

* **Online**, with the web interface at http://haskell.org/hoogle
* **In [IRC](http://haskell.org/haskellwiki/Haskell_IRC_channel)**, using the [Lambdabot](http://haskell.org/haskellwiki/Lambdabot) plugin with `@hoogle` and `@hoogle+`
* **[Installed locally](https://github.com/ndmitchell/hoogle/blob/master/docs/Local-Install.md)**, with either a command line or in a browser
* **[As a developer](https://github.com/ndmitchell/hoogle/blob/master/docs/Developers.md)**, through Haskell or JSON APIs.

# Searches

## Searching

Here are some example searches:

* `map` searches as text, finding `map`, `concatMap`, `mapM`
* `con map` searches for the text "map" and "con" finding `concatMap`, but not `map`
* `a -> a` searches by type, finding `id :: a -> a`
* `a` searches for the text "a"
* `:: a` searches for the type "a"
* `id :: a -> a` searches for the text "id" and the type "a -> a"


## Scope

By default, searches look at the [Haskell Platform](http://hackage.haskell.org/platform) and [Haskell keywords](http://haskell.org/haskellwiki/Keywords). However, all [Hackage](http://hackage.haskell.org) packages are available to search. As some examples:

* `mode +cmdargs` searches only the "cmdargs" package
* `file -base` searches the Haskell Platform, excluding the "base" package
* `mode +platform +cmdargs` searches both the Haskell Platform and the "cmdargs" package
* `count +missingh` searches only the "MissingH" package - all packages are written in lower-case

With the set of packages you are searching, you can also restrict the set of modules searched:

* `file -System` excludes results from modules such as `System.IO`, `System.FilePath.Windows` and `Distribution.System`
* `fold +Data.Map` finds results in the `Data.Map` module


# Integration

## Command Line Version

To invoke Hoogle type:

    hoogle "[a] -> [b]"

Note the quotes, otherwise you will redirect the output to the file [b].

To ensure you have data files for the Hackage modules, you will first need to
type:

    hoogle data

Which will download and build Hoogle databases.

## Chrome Integration

**As a keyword search:** With a keyword search you can type `h map` directly into the location bar to perform a Hoogle search. Go to the [Hoogle website](http://haskell.org/hoogle/) in Chrome, right-click in the Hoogle search field and select "Add as a search engine...". Give it a keyword such as "h".


## Firefox Integration

**From the search bar:** Go to the [Hoogle website](http://haskell.org/hoogle/) in Firefox and click on the drop-down arrow at the left of the search bar, and select the "Add Hoogle" option. Click the arrow again to select Hoogle as your search engine.

**As a keyword search:** With a keyword search you can type `h map` directly into the location bar to perform a Hoogle search. Go to the [Hoogle website](http://haskell.org/hoogle/) in Firefox, right-click in the Hoogle search field and select "Add a Keyword for this Search...". Given it a keyword such as "h".

If you want to search for special symbols in Firefox keyword search, modify the keyword search URL to be: `javascript:window.location.href="http://haskell.org/hoogle?q=" + encodeURIComponent("%s")`


## Firefox Ubiquity Integration

[Ubiquity](https://wiki.mozilla.org/Labs/Ubiquity) provides a graphical command-line for Firefox. To install the Ubiquity Hoogle command, visit the [this page](http://www.randomhacks.net/git/ubiquity/hoogle/) and click "Subscribe..." when asked whether you want to install it. Further information is available [here](http://www.randomhacks.net/articles/2008/09/01/ubiquitous-hoogle).


# Background

Hoogle work is licensed under the [GPL version 2.0](https://github.com/ndmitchell/hoogle/blob/master/docs/LICENSE). Any patches are assumed to be dual licensed under the BSD license and the GPL, to allow re-licensing Hoogle under the BSD license in future, if that proves beneficial to the Haskell community. The work is intended to be helpful, open and free. If the license doesn't meet your needs then talk to me.

## Theoretical Foundations

A lot of related work was done by Rittri [1] and Runciman [2] in the late 80's. Since then Di Cosmo [3] has produced a book on type isomorphisms. Unfortunately the implementations that accompanied the earlier works were for functional languages that have since become less popular.

1. [Mikael Rittri, Using Types as Search Keys in Function Libraries](http://portal.acm.org/citation.cfm?id=99384). Proceedings of the fourth international conference on Functional Programming languages and Computer Architecture: 174-183, June 1989.
2. [Colin Runciman and Ian Toyn, Retrieving reusable software components by polymorphic type](http://portal.acm.org/citation.cfm?id=99383). Journal of Functional Programming 1 (2): 191-211, April 1991.
3. [Roberto Di Cosmo, Isomorphisms of types: from lambda-calculus to information retrieval and language design](http://www.pps.jussieu.fr/~dicosmo/Publications/ISObook.html). Birkhauser, 1995. ISBN-0-8176-3763-X

I have given several presentations on type searching all available from [my home page](http://community.haskell.org/~ndm/hoogle).

## Folders

The folders in the distribution, and their meaning are:

data - tools to generate a hoogle data file
docs - documentation on hoogle
misc - presentations, icons, emacs scripts, logos
src  - source code
web  - additional resources for the web front end (css, jpg etc.)

## Similar Tools

I was unaware of any similar tools before starting development, and no other tool has really influenced this tool (except the first on this list). Some related tools are:

* [Google](http://www.google.com/), the leader in online search
* [Hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html), similar to Hoogle, but with less focus on type search
* [Krugle](http://www.krugle.com/), search code, but no Haskell :(


## Acknowledgements

All code is all &copy; [Neil Mitchell](http://community.haskell.org/~ndm/), 2004-present. The initial version was done over my summer holiday, and further work was done during my PhD. During Summer 2008 I was funded to full-time on Hoogle by [Google Summer of Code](http://code.google.com/soc/) with the [haskell.org](http://haskell.org/) mentoring organisation. Since then I have been working on Hoogle in my spare time. Various people have given lots of useful ideas, including my PhD supervisor [Colin Runciman](http://www.cs.york.ac.uk/~colin/), and various members of the [Plasma group](http://www.cs.york.ac.uk/plasma/). In addition, the following people have also contributed code or significant debugging work:

* [Thomas "Bob" Davie](http://www.cs.kent.ac.uk/people/rpg/tatd2/)
* [Don Stewart](http://www.cse.unsw.edu.au/~dons/)
* Thomas Jager
* [Gaal Yahas](http://gaal.livejournal.com/)
* [Mike Dodds](http://www-users.cs.york.ac.uk/~miked/)
* [Niklas Broberg](http://www.cs.chalmers.se/~d00nibro/)
* Esa Ilari Vuokko
* Udo Stenzel
* [Henk-Jan van Tuyl](http://members.chello.nl/hjgtuyl/)
* Gwern Branwen
* Tillmann Rendel
* David Waern
* Ganesh Sittampalam
* Duncan Coutts
* Peter Collingbourne
* Andrea Vezzosi
* Ian Lynagh
* [Alfredo Di Napoli](http://www.alfredodinapoli.com)

In previous versions, all the data was taken from [Zvon's Haskell Guide](http://www.zvon.org/other/haskell/Outputglobal/). Thanks to their open and friendly policy of allowing the data to be reused, this project became possible. More recent versions use the Hierarchical Libraries as distributed with GHC, and databases generated by Haddock.
