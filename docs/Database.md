# Hoogle Database Format

_This document is forward looking and describes the proposed format for Hoogle 5._

There are packages (e.g. `base`, `hlint`, `mtl`, `parsec`) and package sets (e.g. `platform`, `installed`, `all`). For each package there will be a `.idx` file. For each package or package set there will be a `.str` and `.typ` file. The rest of the document describes the contents and way things are searched.

### `.idx` file

A `.idx` file is the index file. It stores information about each searchable component. As an example, `base.idx` might the entry for `not`:

    module: Data.Bool, GHC.Class, Prelude
    text: <a href class=name>not</a> :: <a href class=type1>Bool</a> -> <a href class=type1>Bool</a>
    source: http://.../GHC.Classes.html#not
    docs: These represent True and False.

Each entry has an id which is it's position in the file, which we write `&not` for the rest of this document. This information is lightly highlighted inside `name` and `type1` etc, but otherwise just displayed directly. It is never used for structured searching. The module names are used for after-the-fact module filtering.

### `.str` file

A `.str` file is a list of the searchable strings. When searching, you want to return:

* Things that match, case sensitive
* Things that match, case insensitive
* Things that match prefix, case sensitive
* Things that match prefix, case insensitive
* Things that match infix, case irrelevant (as camel case breaks it)

The data structure is:

    strings: FMIndex
    string-info: [[(CaseInformation, [&#Address])]]
    unaddress :: &#Address -> (Package, &Address)
    type CaseInformation = BitVector, max 32, which places are capital letters

Given a search term `Foo` we search the `FMIndex` for `|foo|`, then `|foo`, then `foo` - stopping once we have enough answers. We associate an index in `string-info` with each `|` character in the `FMIndex`. For those which match we consult the case information to see how good the match is.

### `.typ` file

A `.typ` file stores information about searchable types. Before a type can be searched we:

* Expand all type aliases everywhere
* Expand all MPTC, so only single parameter type classes remain
* Eliminate all "free" type classes (Data, Tyepable, Eq, Ord, Hash...)
* Instantiate all SPTC into variables, e.g. Domain d => d, becomes Domain

To search, we first look at the "interesting" data types. If a function has _i_ interesting data types then we first look at functions with all _i_, then slowly allow reducing the number of interesting types. In particular, if a function has 0 interesting types, we only ever search the functions with no interesting types.

Within each bucket we rank as either plausible or not, then sort the plausible ones.
