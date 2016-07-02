# TypeSearch algorithm

This document outlines the type search algorithm used in Hoogle.

The basic question is:

    (?) :: TypeQ -> TypeA -> Maybe Cost

Where `TypeQ` is the user query, `TypeA` is drawn from the database, `Nothing` is no match, and `Just i` is a cost of `i` (`Cost` is probably a synonym for `Double`).

## Rewrites

Given `a ? b`, we answer with a cost by applying rewrites until we get to equal values. If we never get to equal values we say `Nothing`, if we arrive at equal values we say `Just` with a cost based on the rewrites applied. The rewrites are:

* **Both:** _[Arg reorder]_ `x -> y -> z ~> y -> x -> z`.
* **Both:** _[Arg delete]_ `x -> y -> z ~> y -> z`, at most once per side.
* **Both:** _[Freevar rename]_ `x ~> y`.
* **LHS only:** _[Alias follow]_ `type X = Y` provides a rewrite `X ~> Y`.
* **LHS only:** _[Instance subtype]_ `instance C X` provides a rewrite `X ~> forall z . C z`.
* **RHS only:** _[Context delete]_ `C x => y ~> y`.
* **Both:** _[Special]_ `Maybe a ~> a` can be applied, along with a few other special rules.

## Optimisation

We optimise the rewrites by providing some operations like `?` but which work on precomputed information and can be applied cheaply. In general, we define:

    (??) :: TypeQ? -> TypeA? -> Bool

Where `False` predicts that `?` will return `Nothing`, but cheaper.

### Arity

Compute `TypeQ#` and `TypeA#` by taking the arity of the type.

    (?#) :: TypeQ# -> TypeA# -> Bool
    i ?# j = if i == 0 || j = 0 then i == j else abs (j - i) <= 1

Works because we can only apply _[Arg delete]_ at most once. For the special case where either side is arity 0 (a CAF) we demand equality.

### Rarity

Compute `TypeQ!` and `TypeA!` by taking the number of packages/modules/signatures each constructor or context occurs in, and finding the lowest (rarest) value. For `TypeA!` also consider following all instance and alias rules.

    (?!) :: TypeQ! -> TypeA! -> Bool
    i ?! j = i >= j * 3

The rationale is that if you search for a common search string like `Monad m => m a -> IO a` (all symbols found in almost all packages) then anything containing a rare value like `ShakeOptions` should be excluded, even if it happens to match.

Do not follow instances that are considered to be "uninteresting", such as `Eq`, `Data`, `Typeable` etc (otherwise very rare types get marked as very common).

### Names

Compute `TypeA~` as the list of constructors and contexts that occur, tagging the contexts with a leading `#` to ensure they do not clash with constructors. Sort the list of names.

Compute `TypeQ~` as a decision tree of names that must occur to match, relying on the fact that the only "LHS only" rules can introduce new names. As some examples:

* If constructor `Foo` (no alias or instances) is mentioned it must be literally present.
* If constructor `FilePath` (`type FilePath = String`) is mentioned either `FilePath` must be present or `String` must be satisfied.
* If `String` (`type String = [Char]`) is mentioned either `String` must be present or `[]` and `Char`.
* If `[]` (`instance Functor []`) is mentioned either `[]` must be present or `Functor` must be satisfied.

Compute `?~` by generating a state machine from the tree of `TypeQ~` and applying it to `TypeA~`.


# Examples

* `a -> b` should only look at things with very rare constructors - perhaps a bit of `()` and `Eq`, but nothing more.
* `ShakeOptions -> ()` can look at very rare things, as long as they contain `ShakeOptions` in them.
* `ShakeOptions -> ()` has a `Data` instance, so `{Data} -> ()` is fine to search for, but then you are restricted to things that have a very common things in them, since `Data` is so common.

# Random notes

## Model

The model is we have concrete types, type variables, and properties over type variables (roughly contexts).

    Functor f => (a -> b) -> f a -> f b

Here `Functor` is a property of some type variable. We can reduce that with the equations `{} => Functor []`. We also have rules such as `{Ord a} => Ord [a]`.

There are also aliases, e.g. `String => FilePath`.

In some ways, this is just proof search?

* **LHS only:** _[Alias follow]_ `type X = Y` provides a rewrite `X ~> Y`.
* **LHS only:** _[Instance subtype]_ `instance C X` provides a rewrite `X ~> forall z . C z`.

Given a concrete type, which aliases can you follow?

**Not satisfying...**

Semantically...


A generic instaniable proof system, where:

How close are two proofs?

Do they talk about the same things? Are the things easily convertable?

apply some rules, which rules got used. can i do bulk apply? things that are only one rule apart? can i start small and evolve upwards?

classifiers?

ShakeOptions is a pretty unique classifier, only 26 odd have it.

IO () return type is a poor classifier, very common.


## Properties

For any type T present in the result, searching for it should give that result first.

# More thoughts

Just need to get something working - anything really!

Aliases have a great problem with matching too much...

Names that clash really screw things up...

Monad => IO ...


    (a -> b) -> [a] -> [b]

    Functor f => (a -> b) -> f a -> f b

If I ask for Functor f ... - I probably really want to have some functor context.

If I ask for [], I can probably generalise it

A virtual machine for matching? Precompute a _program_ that makes the decision.

This set of functions all require you to have written `Functor` as a context.


Given:

> ShakeOptions -> Action a -> IO a

Searching:

> Action a -> IO a

The rarest thing in the first is ShakeOptions. The second rarest is Action.

Add a characteristic function on each thing?

44K functions. Can do a moderate amount of computation on each one.
vs 252K names.

Best thing to do is get down to 100 or so plausible ones, then go from there.

Arity filters.
