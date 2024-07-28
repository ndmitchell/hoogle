# Hoogle [![Hackage version](https://img.shields.io/hackage/v/hoogle.svg?label=Hackage)](https://hackage.haskell.org/package/hoogle) [![Stackage version](https://www.stackage.org/package/hoogle/badge/nightly?label=Stackage)](https://www.stackage.org/package/hoogle) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/hoogle/ci.yml?branch=master)](https://github.com/ndmitchell/hoogle/actions)

Hoogle is a Haskell API search engine, which allows you to search many standard Haskell libraries by either function name, or by approximate type signature. The online version can be found at https://hoogle.haskell.org/ and searches [Stackage](https://www.stackage.org/).

* **Online version:** https://hoogle.haskell.org/
* **Hackage page:** https://hackage.haskell.org/package/hoogle
* **Source code:** https://github.com/ndmitchell/hoogle
* **Bug tracker:** https://github.com/ndmitchell/hoogle/issues

## Hoogle Use

Hoogle can be used in several ways:

* **Online**, with the web interface at https://hoogle.haskell.org/
* **In [IRC](https://wiki.haskell.org/IRC_channel)**, using the [Lambdabot](https://wiki.haskell.org/Lambdabot) plugin with `@hoogle` and `@hoogle+`
* **From `emacs`**, by means of [`engine-mode`](https://github.com/hrs/engine-mode)
* **[Installed locally](./docs/Install.md)**, with either a command line or in a browser
* **[As a developer](./docs/API.md)**, through Haskell or JSON APIs.

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

By default, searches look at the [Haskell Platform](https://www.haskell.org/platform/) and [Haskell keywords](https://wiki.haskell.org/Keywords). However, all [Stackage](https://stackage.org) packages are available to search. As some examples:

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

    $ hoogle "[a] -> [b]"

Note the quotes, otherwise you will redirect the output to the file [b].

To ensure you have data files for the Hackage modules, you will first need to
type:

    $ hoogle generate

Which will download and build Hoogle databases.

## Command Line UI

There is a terminal/curses based UI available through [`cabal install bhoogle`](https://hackage.haskell.org/package/bhoogle).

## Chrome Integration

**As a keyword search:** With a keyword search you can type `h map` directly into the location bar to perform a Hoogle search. Go to the [Hoogle website](https://hoogle.haskell.org/) in Chrome, right-click in the Hoogle search field and select "Add as a search engine...". Give it a keyword such as "h".

## Firefox Integration

**From the search bar:** Go to the [Hoogle website](https://hoogle.haskell.org/) in Firefox and click on the `⋯` symbol at the right of the URL bar, and select the "Add Search Engine" option. Click the hoogle logo at the bottom of the completion dropdown when searching to perform a Hoogle search.

**As a keyword search:** With a keyword search you can type `h map` directly into the location bar to perform a Hoogle search. Go to the [Hoogle website](https://hoogle.haskell.org/) in Firefox, right-click in the Hoogle search field and select "Add a Keyword for this Search...". Given it a keyword such as "h".

## Others

* [Doc Browser](https://github.com/qwfy/doc-browser)

### The Source Code

    $ git clone https://github.com/ndmitchell/hoogle.git

Contributions are most welcome. Hoogle is written in Haskell 98 + Heirarchical Modules, I do not wish to change this. Other than that, I'm pretty flexible about most aspects of Hoogle. The [issue tracker](https://github.com/ndmitchell/hoogle/issues) has many outstanding tasks, but please contact me if you have thoughts on doing something major to Hoogle, so I can give some advice.

# Background

Hoogle work is licensed under the [BSD-3-Clause license](https://github.com/ndmitchell/hoogle/blob/master/LICENSE).

## Theoretical Foundations

A lot of related work was done by Rittri [1] and Runciman [2] in the late 80's. Since then Di Cosmo [3] has produced a book on type isomorphisms. Unfortunately the implementations that accompanied the earlier works were for functional languages that have since become less popular.

1. [Mikael Rittri, Using Types as Search Keys in Function Libraries](https://doi.org/10.1145/99370.99384). Proceedings of the fourth international conference on Functional Programming languages and Computer Architecture: 174-183, June 1989.
2. [Colin Runciman and Ian Toyn, Retrieving reusable software components by polymorphic type](https://doi.org/10.1145/99370.99383). Journal of Functional Programming 1 (2): 191-211, April 1991.
3. [Roberto Di Cosmo, Isomorphisms of types: from lambda-calculus to information retrieval and language design](https://doi.org/10.1145/270563.571468). Birkhauser, 1995. ISBN-0-8176-3763-X

I have given several presentations on type searching all available from [my home page](https://ndmitchell.com/).

## Project Structure

The folders in the repository, and their meaning are:

cbits             - C implementation of the text search used by hoogle

docs              - documention on hoogle

html              - resources for hoogle's web front-end (html, css, javascript, images, etc.)

misc              - scripts, logos, sample data, etc.

src               - haskell source code

## Similar Tools

I was unaware of any similar tools before starting development, and no other tool has really influenced this tool (except the first on this list). Some related tools are:

* [Google](https://www.google.com/), the leader in online search
* [Hayoo](https://hackage.haskell.org/package/Hayoo), similar to Hoogle, but with less focus on type search
* [Krugle](https://www.krugle.com/), search code, but no Haskell :(
* [Cloogle](https://cloogle.org), for the [Clean](https://clean.cs.ru.nl/Clean) language


## Acknowledgements

All code is all &copy; [Neil Mitchell](https://ndmitchell.com/), 2004-present. The initial version was done over my summer holiday, and further work was done during my PhD. During Summer 2008 I was funded to full-time on Hoogle by [Google Summer of Code](https://summerofcode.withgoogle.com/) with the [haskell.org](https://www.haskell.org/) mentoring organisation. Since then I have been working on Hoogle in my spare time. Various people have given lots of useful ideas, including my PhD supervisor [Colin Runciman](https://www-users.cs.york.ac.uk/~colin/), and various members of the [Plasma group](https://www.cs.york.ac.uk/plasma/wiki/). In addition, the following people have also contributed code or significant debugging work:

* Thomas "Bob" Davie
* Don Stewart
* Thomas Jager
* [Gaal Yahas](https://gaal.livejournal.com/)
* Mike Dodds
* Niklas Broberg
* Esa Ilari Vuokko
* Udo Stenzel
* [Henk-Jan van Tuyl](https://github.com/HJvT)
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

# Interesting links

* https://atom.io/packages/haskell-hoogle
* https://hackage.haskell.org/package/hoogle-index
