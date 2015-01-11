# TypeSearch algorithm

The basic question is:

    (?) :: TypeQ -> TypeA -> Maybe Cost

Where `TypeQ` is the user query, `TypeA` is drawn from the database, `Nothing` is no match, and `Just i` is a cost of `i` (`Cost` is probably a synonym for `Double`).

## Rewrites

Given `a ? b`, we answer with a cost by applying rewrites until we get to equal values. If we never get to equal values we say `Nothing`, if we arrive at equal values we say `Just` with a cost based on the rewrites applied. The rewrites are:

* **Both:** _[Arg reorder]_ `x -> y -> z ~> y -> x -> z`.
* **Both:** _[Arg delete]_ `x -> y -> z ~> y -> z`, at most once per side.
* **Both:** _[Freevar rename]_ `x ~> y`. 
* **LHS only:** _[Alias follow]_ * `type X = Y` provides a rewrite `X ~> Y`.
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
    i ?# j = abs (j - i) <= min 1 i

Works because we can only apply _[Arg delete]_ at most once. For the special case where either side is arity 0 (a CAF) we demand equality.

### Rarity

Compute `TypeQ!` and `TypeA!` by taking the number of signatures (before or after nub?) each constructor or context occurs in, and finding the lowest (rarest) value. For `TypeA!` also consider following all instance and alias rules (but make sure you treat that as a max).

    (?!) :: TypeQ! -> TypeA! -> Bool
    i ?! j = i >= j * 3

The rationale is that if you search for `Monad m => m a -> IO a` then anything containing a rare value like `Action` should be excluded, even if it happens to match.

### Constructors

