# state-applicatives

_The original impetus for this discussion, and the adapted code for `filtering`, `distinct` etc., were due to working through the System F functional programming course in Haskell, [fp-course](https://github.com/system-f/fp-course)_

Given the following:

```
newtype State s a = 
	State { runState :: s -> (a, s) }

exec :: State s a -> s -> s
exec mkState = snd . runState mkState

eval :: State s a -> s -> a
eval mkState = fst . runState mkState

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

instance Functor (State s) where
	(<$>) :: (a -> b) -> State s a -> State s b
	f <$> st = State $
		\x -> let (y, 	t) = runState st $ x
			  in  (f y, t)
```

There are several different ways to write the body of `<*>`, the "apply" operator, which typecheck. The `Applicative` typeclass is defined as:

```
class Functor k => Applicative k where
  pure ::
    a -> k a
  (<*>) ::
    k (a -> b)
    -> k a
    -> k b
```

But how many of those ways satisfy the `Applicative` laws?

```
-- * The law of identity
--   `∀x. pure id <*> x = x`
--
-- * The law of composition
--   `∀u v w. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
--
-- * The law of homomorphism
--   `∀f x. pure f <*> pure x = pure (f x)`
--
-- * The law of interchange
--   `∀u y. u <*> pure y = pure ($ y) <*> u`
```

[Counting possible implementations](#counting-possible-implementations)
[Merely lawful vs. canonical Applicative for `State s :: * -> *` type constructor](#merely-lawful-vs-canonical-applicative-for-state-s------type-constructor)
[Applicatives as contexts](#applicatives-as-contexts)
[Implementation of filtering & an example using Maybe applicative](#implementation-of-filtering--an-example-using-maybe-applicative)
[The State s applicative & an example with tracked-state-as-context](#the-state-s-applicative--an-example-with-tracked-state-as-context)
[Completing the analogy of "values seen so far" context as an applicative](#completing-the-analogy-of-values-seen-so-far-context-as-an-applicative)
[Mechanics of the State s applicative](#mechanics-of-the-state-s-applicative)

***

## Counting possible implementations

There are various ways of writing an implementation for `(<*>)` that will type-check. Instead of exhaustively listing out these possibilities,<sup><a href="#fn1" id="ref1">1</a></sup> we may limit ourselves to considering just those implementations which use each of `sf`, `sx` exactly once, to produce terms `:: a -> b`, `:: a` respectively (needed for the `:: b` value) as well as produce states (which we have to decide what to do with.) Our rough argument in favor of considering _only_ the implementations which use each of `sf`, `sx` exactly once could be seen as an appeal to symmetry: we wouldn't want the operator `(<*>)` to "prefer" either input over the other for no clear reason, unless we later came across a reason to do otherwise. Both inputs `sf`, `sx` should participate in the overall process to an equal extent.

With this principle in mind, we could list out the possibilities as follows: 

```
1. Use `sf` first, then `sx` on the resulting state. In the state part of the output pair, use 
  a. `s`
  b. `exec sf $ s`
  c. `exec sx $ exec sf $ s`
2. Use `sx` first, then `sf` on the resulting state. In the state part of the output pair, use
  a. `s`
  b. `exec sx $ s`
  c. `exec sf $ exec sx $ s`
3. Use `sf`, `sx` both at the same time. In the state part of the output pair, use
  a. `s`
  b. `exec sf $ s`
  c. `exec sx $ s`
```

From these 9 implementations, only 2 are entirely lawful Applicative instances: 1c. and 2c. This immediately suggests that what these two implementations have in common, namely their "threading-through" of an initial state through each step of the computation, resulting in a final reported state that involves the execution of both `sf` and `sx`, is somehow central to what it means to be an Applicative instance. This point will be revisited later below.

## Merely lawful vs. canonical Applicative for `State s :: * -> *` type constructor

Despite the existence of multiple ways to implement the apply operation, `(<*>)`, for `State s` that satisfy the Applicative laws (as verified using `quickcheck` / `checkers`), the canonical implementation<sup><a href="#fn2" id="ref2">2</a></sup> is in fact:

```
sf (<*>) sx = State $ \s ->
  let (f, s' ) = runState sf $ s
      (x, s'') = runState sx $ s'
  in  (f x, s'')

```

However, a more persuasive argument in favor of this implementation might be an illustration of how this one succeeds, where others fail. In order to lay the ground for such a demonstration, it will be useful first to motivate and give an example of how applicative instances serve to provide a _context_ surrounding values and computations performed using them.

## Applicatives as contexts

Some suggestive language to describe the context corresponding to certain simple applicatives might look like:

| `k :: * -> *` | `Applicative k :: * -> *`   | Description |
| :---          | :---                        | :--- |
| `[]`          | `Applicative []`            | The applicative representing _nondeterminism_: zero, one or many values | 
| `Maybe`       | `Applicative Maybe`         | The applicative representing _potential failure_: zero values in case of failure, or one value in case of success |
| `State s`     | `Applicative (State s)`     | The applicative representing _tracking a state_: as a result is being computed, a state is being kept track of, and made available for use by the computation |

That an applicative can serve as some sort of context "surrounding" a value is at least made possible by the fact that it is a typeclass which admits as members higher-kinded types of kind `* -> *`: however, the nature of that context will be determined by the implementation of the type constructor `k`'s `Applicative` instance.

For example, the familiar

```
filter :: (a -> Bool) -> [a] -> [a]
```

operates in an expression _e.g._ `filter pred xs` so as to keep only the sublist of `xs` whose elements satisfy the predicate `pred`, discarding them otherwise. It is equivalent to defining

```
foldr 
  (\x xs -> (\b y ys -> if b then y:ys else ys)
            (pred x)
            x
            xs)
  []
  xs
```

or, more succinctly by inlining `pred x`, `x`, `xs`:

```
foldr 
  (\x xs -> if pred x then x:xs else xs)
  []
  xs
```

which in turn can be viewed like replacing every `(:)` along the spine of the input list `xs` with an infixed application of the lambda `:: a -> [a] -> [a]` used by `foldr`:

```
x1 `(\a as -> ...)` $
  x2 `(\a as -> ...)` $
   ...                  $
    xn `(\a as -> ...)` []
```

Another way to say this is that `filter` is obtained by specializing the `foldr` catamorphism so as to create a sublist of elements, all of which must satisfy the predicate given to `filter`.

Compare this now with `filtering`:

```
filtering :: (a -> k Bool) -> [a] -> k [a]
filter    :: (a ->   Bool) -> [a] ->   [a]
```

whose type signature suggests that the predicate for picking out the sublist, and the resulting sublist itself, are evaluated and built up (respectively) _within a context that is realized by the implementation of `k`'s applicative instance._ We can consider certain examples of `k` to give this a concrete meaning:

- for `k := []`, the predicate now produces a value in the surrounding context of _nondeterminism_: instead of committing to a single boolean value, a list of such results, so that the final result, `:: [[a]]`, represents a _list of possible sublists_, also without committing to a single result.
- for `k := Maybe`, the predicate now produces a value in the surrounding context of _potential failure_: a `Nothing` may arise from the computation of any term, and "short-circuit" the overall computation (whose output type `Maybe [a]` represents only the _possibility_ of a list, with potential failure) as a result.
- for `k := State s`, `s` a `*`-kinded type, the predicate now produces a value in the surrounding context of _a state being tracked_ throughout the course of the computation (such an example will be developed in more detail below.)

We turn our attention to the body of `filtering` (which further highlights its similarity to `filter`), and develop the example where `k := Maybe` applicative.

## Implementation of `filtering` & an example using `Maybe` applicative

```
filtering :: Applicative k =>
             (a -> k Bool) -> [a] -> k [a]
filtering kpred lst =
  foldr (\x kxs -> pure (\b y ys -> if b then y:ys else ys)
                   <*> kpred x 
                   <*> pure x 
                   <*> kxs)
        (pure [])
        lst
```

The (verbose) implementation for `filter` is reproduced for convenience:

```
filter pred lst =
  foldr (\x xs -> (\b y ys -> if b then y:ys else ys)
                  (pred x)
                  x
                  xs)
        []
        xs
```

Notice first of all that in the `filtering` example we no longer have recourse to immediately inlining `kpred x` for the parameter `b` in the lambda body which checks whether to keep or discard its `y :: a` value. (There are syntactic conveniences available _e.g._ the assorted applicative lifts, via `Control.Applicative`, but I'm setting those aside for right now.) 

Let's walk through an example, approaching this in "foldr as `(:)`-replacement" terms, as was discussed above. In evaluating

```
filtering
  (\a -> if a > 5 then Nothing else Just (a <= 2))
    [1, 3, 6]
```

we consider that this activity is like evaluating

```
foldr (\x kxs -> pure (\b y ys -> if b then y:ys else ys)
                 <*> (if x > 5 then Nothing else Just (x <= 2)) 
                 <*> pure x 
                 <*> kxs)
      (pure [])
      (1 : 3 : 6 : [])
```

which we can begin to evaluate by re-writing to

```
1 `(\x kxs -> ...)` $ 
  foldr (\x kxs -> ...) (pure []) (3 : 6 : [])
```

We can now substitute `1` and `foldr ...`, the pending part of the computation, for `x` and `kxs` respectively:

```
pure (\b y ys -> if b then y:ys else ys)          -- Maybe (Bool -> a -> [a] -> [a])
  <*> (if 1 > 5 then Nothing else Just (1 <= 2))  -- Maybe  Bool 
  <*> pure 1                                      -- Maybe          a
  <*> (foldr ...)                                 -- Maybe               [a]
```

The applicative instance for `Maybe` works as follows, which is consistent with the concept of `Applicative Maybe` as representing a context of possible failure (in particular, the application of maybe-a-function to maybe-a-value fails unless there happens to be both a function and a value):

```
instance Applicative Maybe where
  pure  :: a -> Maybe a
  pure  =  Just
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b 
  mf <*> mx = case (mf, mx) of
    (Nothing,  _)       -> Nothing
    (_,        Nothing) -> Nothing
    (Just f,   Just x)  -> Just (f x)
```

Evaluating subexpressions, and re-writing `pure`'s for our specific situation of `k := Maybe`:

```
Just (\b y ys -> if b then y:ys else ys) 
  <*> Just True 
  <*> Just 1  
  <*> (foldr ...)   
```

So because `(if 1 > 5 then Nothing else Just (1 <= 2)) == Just True`, `1` is _retained in_ the result:

```
(Just (1:)) <*> $                                 -- i.e. (Just (\ys -> 1:ys))
  foldr (\x kxs -> ...) (pure []) (3 : 6 : [])
```

```
(Just (1:)) <*> $
  3 `(\x kxs -> ...)` $ 
    foldr (\x kxs -> ...) (pure []) (6 : [])
```

and we can repeat the above to see what the infixed `(\x kxs -> ...)` on `3` does:

```
pure (\b y ys -> if b then y:ys else ys)    
  <*> (if 3 > 5 then Nothing else Just (3 <= 2)) 
  <*> pure 3   
  <*> (foldr ...)    
```

Because `(if 3 > 5 then Nothing else Just (3 <= 2)) == Just False`, `3` is _discarded from_ the result:

```
(Just (1:)) <*> $
  (Just id) <*> $                                 -- i.e. (Just (\ys -> ys))
    foldr (\x kxs -> ...) (pure []) (6 : [])
```

```
(Just (1:)) <*> $
  (Just id) <*> $  
    6 `(\x kxs -> ...)` $ 
    foldr (\x kxs -> ...) (pure []) []
```

But this time the infixed `(\x kxs -> ...)` on `6` shows us:

```
pure (\b y ys -> if b then y:ys else ys)    
  <*> (if 6 > 5 then Nothing else Just (6 <= 2)) 
  <*> pure 6   
  <*> (foldr ...)    
```

Because `(if 6 > 5 then Nothing else Just (6 <= 2)) == Nothing`, _the entire computation now short-circuits._ What feature of the `Maybe` applicative gives rise to this behavior? Precisely the fact that, when evaluating subexpressions in the order in which `(<*>)`, left-infixing, forces them, we arrive at:

```
(pure (\b y ys -> if b then y:ys else ys) <*> Nothing) <*> pure 6 <*> (foldr ...)
                                             (Nothing <*> pure 6) <*> (foldr ...)
                                                        (Nothing <*> (foldr ...))
                                                                          Nothing
```

and the outcome of evaluating this "innermost" subresult, in turn, short-circuits the entire result sublist that had been in progress:

```
(Just (1:)) <*> $
  (Just id) <*> $  
    Nothing
```

where the failure propagates back out, giving the ultimate result of `Nothing :: Maybe a`.

## The `State s` applicative & an example with tracked-state-as-context

What use could we get out of `filtering`, defined above, using the applicative of `State s` for some concrete type s? And how would we want to _implement_ that applicative, in order to use it for that?

Suppose we want to write a function that will take a list and return the sublist consisting only of the _distinct_ elements present in it (any repetitions discarded.) (The sublist need not be shorter than the original list, _e.g._ if the sublist contains no repetitions.) This should at least have the type: `distinct :: [a] -> [a]`, though we may find it useful or necessary to further constrain the type `a` as we proceed.

```
-- distinct [1, 2, 3, 2, 1] == [1, 2, 3]
distinct :: [a] -> [a]
```

The following naïve description is motivated by considering how one might do this "by hand" on a stack of face-up playing cards, `as := a1 : a2 : ... an`, with `a1` on top to start with and resting on top of the rest of the stack, `tail as`:

- check the value of `a1`, and if it matches a value already observed in the course of working out what the final value will be, discard it, but keep it otherwise;
- check the value of `a2`, and if it matches a value already observed in the course of working out what the final value will be, discard it, but keep it otherwise;
- (so on, finitely many times, until `an` at the bottom of the stack is reached)

Notice that "in the course of working out what the final value will be" refers to the course of the _overall computation_, which proceeds from the outside of the list on, towards the innermost value; this is the reason why we have `distinct [1, 2, 3, 2, 4, 1] == [1, 2, 3, 4]` rather than `[3, 2, 4, 1]`.

What this suggests is that we can employ a similar strategy to that which `foldr` and `filtering` have used, _i.e._ the strategy of replacing the "spine" of the list, built by right-infixing `(:)`s, with some suitable operation that builds up a result starting from the innermost `(:)`-cell (that is, from the bottom of the stack of cards on up, moving through the stack back out to the top.)

However, and in a spirit similar to how _any_ value along the spine (in the `filtering` example above) could prompt the entire computation to short-circuit by giving an `Nothing`, thus relaying (to recursive `foldr` calls building up a result) secondary information _about_ the computation in progress, we will want for the `(:)`-replacing infixed operation to be capable of relaying _cumulative_ information about the `aj`'s, `j := 1, 2, ..., n` encountered thus far: namely, the _set_<sup><a href="#fn3" id="ref3">3</a></sup> of values encountered among the elements of `as` that have been read off thus far (so that any elements matching already encountered values can be discarded.)

That is to say: instead of checking each element, as we encounter it, and seeing whether it satisfies some predicate _in a context of potential failure_ (`k := Maybe`), we are checking each element as we encounter it, but seeing whether it satisfies some predicate _with respect to a cumulative set-of-values-seen, being maintained in context_ (`k := ?`). The predicate in question is _set membership_, but we are additionally concerned with _adding members not previously encountered to the set_: this is precisely the "set maintenance in context" alluded to above. We know enough to at least indicate a type for the activity being applied to each element of the original input set:

```
a -> Set a -> (Bool, Set a)
```

In fact, through use of the set operations provided in the modeul `Data.Set`, we can write:

```
import qualified Data.Set as S

-- ...

checkAndMaintainSet :: a -> Set a -> (Bool, Set a)
checkAndMaintainSet    x =
                            \set -> if x `S.member` set   -- if the element has already been encountered
                                      then (False,        -- then do *not* keep it in the result being built up
                                              set)        -- and leave the set unchanged
                                      else (True,                 -- otherwise, *do* keep it in the result being built up
                                              x `S.insert` set)   -- and add this element to the set
```

This code is responsible, not for _building_ up the eventual result, but for _telling that piece of code_ which is responsible what to do with one element at a time, while passing along updated information to the process to compute the pending "rest of the computation." Now what is that piece of code? In an "unlifted setting" it's precisely 

```
-- :: Bool -> a -> as -> as
      (\b     y    ys -> if b then y:ys else ys)
```

But we already have seen how, in the context of possible failure (`k := Maybe`), we had to lift this operation (which "dispatches" the proper thing to do with the current element, in the `a` parameter) as follows:

```
pure (\b y ys -> if b then y:ys else ys) :: k (Bool -> a -> as -> as)
```

After being "lifted" into applicative context, this code can now be used with the operator for _applying_ functions in applicative context: this is none other than `(<*>)`:

```
(\x kxs -> pure (\b y ys -> if b then y:ys else ys)
                 <*> setpred x                       -- :: Set a -> (Bool, Set a)
                 <*> pure x 
                 <*> kxs)
```

where `setpred` is another name I've just given for `checkAndMaintainSet`, above:

```
setpred :: a -> Set a -> (Bool, Set a)
setpred x = 
  \set -> if x `S.member` set   
           then (False, set)  
           else (True, x `S.insert` set) 
```

To summarize: our predicate for determining whether each element of the input list (one at a time) should go in the final result being built up must also maintain a state (the set of elements seen so far) in a context relevant throughout the duration of the computation. But in order for the code dispatching whether or not to `(:)`, the element under consideration itself, and the pending remainder of the list to participate in computations within that _same_ context, they must be lifted by the use of `pure` if they do not already have a suitable type. We can decide that the empty list upon which we want to add elements, `[]`, gets lifted right away, and this is the idea behind the use of `pure []` in `foldr (\x kxs -> ... <*> setpred x <*> ...) (pure []) lst` _i.e._ `filtering setpred lst`. 

## Completing the analogy of "values seen so far" context as an applicative

There is an obvious difference between this latest notion of context (_e.g._ giving back a boolean in-context) vs. those seen previously:

```
k := Maybe
kpred  :: a -> Maybe Bool

k := []
kpred  :: a -> [] Bool 
--        i.e. [Bool]

k := ?
setpred :: a -> (Set a -> (Bool, Set a))
```

That is: it is not obvious what type constructor we could use for `k`, above, so that `k Bool` will be the type representing booleans in our intended context. But with a bit of wishful thinking, we can start to imagine what `k` could look like:

```
a -> (Set a -> (Bool, Set a))    -- '->' is an infixing type constructor
a -> (->) (Set a) (Bool, Set a)  -- the output type is the result of applying *something* 
                                 -- to the two *-kinded types:      Set a     Bool
a -> (\(x :: *) (y :: *) -> ...) (Set a) (Bool)
                                 -- but what is that *something*?
a -> (\(x :: *) (y :: *) ->      -- It's the type-level function which takes two *-kinded types x, y ...
      ((->) x (y, x)))           -- ... and produces this function type i.e. the type 'x -> (y, x)'
``` 

Try partially applying the type-level function we've imagined into being:

```
(\(x :: *) (y :: *) -> 
  ((->) x (y, x))) (Set a)

(\(y :: *) -> 
  (Set a -> (y, Set a))) 
```

and we have the type-level function _representing the context_ in which a `:: Bool` results from applying the predicate (in fact, applying the later type-level function to `y := Bool` now gives the output type of `setpred`.) 

Type-level functions, and the implementation of typeclasses as type-level functions, are not directly supported in Haskell without the use of language extensions, but the next best thing is easy to do and perfectly suitable for our purposes: we _establish an isomorphism of the typeclass to be implemented with the intended function type_:

```
newtype State s a -> State 
  { runState :: s -> (a, s) }
-- State    :: (s -> (a,s)) -> State s a
-- runState :: State s a    -> (s -> (a,s))
```

Distinguish `State :: * -> * -> *` the _type_ constructor on the left-hand side, with `State` the _data_ constructor on the right-hand side. We now have what we need to complete the analogy:

```
k := Maybe                -- :: * -> *
kpred  :: a -> Maybe Bool

k := []                   -- :: * -> *
kpred  :: a -> [Bool]

k := State (Set a)        -- :: * -> *
setpred :: a -> State (Set a) Bool
```

where the partially-applied type constructor `State (Set a)` now provides us with the type constructor with which to represent our intended "context surrounding a Boolean."

We have to re-write the body of `setpred` so that its output type makes use of what we've just defined:

```
setpred :: a -> State (Set a)
setpred x = State $
  \set -> if x `S.member` set   
           then (False, set)  
           else (True, x `S.insert` set) 
```

## Mechanics of the `State s` applicative

Our situation so far is that we are capable of _representing_ the desired "stateful context" as a type constructor which may be applied to concrete types, but not yet clear on _how to do things_ in that context: for example, if we were to write down the following (making sure to add the `Ord a` type constraint, which is needed if we're going to pass along a `:: Set a` over the course of the computation):

```
distinct :: Ord a => [a] -> ?
distinct lst = 
  filtering         -- :: (a -> State (Set a) Bool) -> [a] -> State (Set a) [a]
    setpred         -- :: (a -> State (Set a) Bool)
    lst             -- ::                              [a]
```

then it's immediately clear that _our expression doesn't have the right type!_ We needed `[a]` but instead have `State (Set a) [a]`. We'll want some way to "extract" that final value from the expression output.<sup><a href="#fn4" id="ref4">4</a></sup> Furthermore, `filtering setpred lst` is precisely

```
foldr (\x kxs -> pure (\b y ys -> if b then y:ys else ys)
                 <*> State (\set -> if x `S.member` set then (False, set) else (True, x `S.insert` set)) 
                 <*> pure x 
                 <*> kxs)
      (pure [])
      lst
```

but we haven't yet decided what it means to "inject" an ordinary term `:: a` into our stateful context, using `pure` to become `:: State s a`; nor what it means to _apply_ a function in stateful context to a value in stateful context, using `(<*>)`. 

We'll attempt to work out an answer to the latter question by working through the first few steps of our answer, for the example list `[1,2,1,3]`. We know already that this amounts to repeatedly infixing, in place of the input list's `(:)`'s, the 1st argument `(\x kxs -> ...)` to `foldr`:

```
1 `(\x kxs -> ...)` $
  foldr (\x kxs -> ...) (pure []) [2, 1, 3]
```

What will the infixed function do with `1`?

```
(\kxs -> pure (\b y ys -> if b then y:ys else ys)
          <*> State (\set -> if 1 `S.member` set then (False, set) else (True, 1 `S.insert` set)) 
          <*> pure 1 
          <*> kxs)
  (foldr (\x kxs -> ...) (pure []) [2, 1, 3])
```

We're supposed to check `1` for membership in the set of values seen already (including it in the final sublist if not yet seen, but discarding it otherwise), and updating said set to include `1` if it wasn't yet seen. But we've only just started looking at the list! So we haven't seen any values yet. The empty set, `S.empty`, is defined in `Data.Set`: it's what mathematicians call `{}`. We sense somehow that _this_ is the proper "set so far", and we certainly want to seed the computation `pure <*> ... <*> (foldr ...)` with it ... but where shall we feed `S.empty` into?

```
pure (\b y ys -> if b then y:ys else ys)                                                  -- :: State (Set a) (Bool -> a -> [a] -> [a])
  <*> State (\set -> if 1 `S.member` set then (False, set) else (True, 1 `S.insert` set)) -- :: State (Set a)  Bool
  <*> pure 1                                                                              -- :: State (Set a)          a
  <*> (foldr ...)                                                                         -- :: State (Set a)                      [a]
```

_Every term in this expression_ (that is, everything that isn't an `(<*>)`) _has a type isomorphic with some function type *from `Set a`*._ 

```
State (Set a) (Bool -> a -> [a] -> [a]) ~ (Set a -> Bool -> a -> [a] -> [a])
State (Set a)  Bool                     ~ (Set a -> Bool)
  , etc.
```

So we should be fine passing `S.empty` to the _first_ term whose evaluation is forced by `(<*>)` (recall that this operator is left-infixing), so long as `(<*>)`, the contextualized function application, can "make that set's resulting state available" to the next term. We'll call this activity _threading the state through._ This means the next thing we have to decide is what a `pure q :: State (Set a) y` term should do with the state given to it.

Well, thinking about this, the only term that's really here in order to maintain the evolving state is `State (\set -> ...)`; everything else here is either being lifted for the sake of participating in the same context as this process (_i.e._ being able to get the state and pass it along to the rest of the computation, without having to change it), or is the pending remainder of the computation (`foldr ... [2, 1, 3]`.) So lifting a term into applicative context using `pure` really means that we don't assign it any responsibility whatsoever for updating the state it receives:

```
pure :: q -> State s q
pure    y =  State $ 
  \set -> (q, set)
```

That just leaves the question of what it will mean to apply a contextualized function to a contextualized term, using `(<*>)`. In any event:

```
(<*>)     :: State s (a -> b) -> State s a -> State s b
sf <*> sx ::                                  State s b
```

so our result should represent some process `s -> (b, s)`. But which process?

```
sf (<*>) sx = State $ \s ->
  ...
```

`sf` and `sx` need not arise from `pure` generally: they may well _not_ be indifferent to the state which either one receives. In that case, we had better pay attention to the state which results from applying each one, in turn:<sup><a href="#fn5" id="ref5">5</a></sup>

```
sf (<*>) sx = State $ \s ->
  let (f, s' ) = runState sf $ s
      ...
```

We have a new state, `s'`: let's make it available to `sx`:

```
sf (<*>) sx = State $ \s ->
  let (f, s' ) = runState sf $ s
      (x, s'') = runState sx $ s'
  in  ...
```

From here, we can conclude what the appropriate result pair `:: (b, s)` should be, if we consider that any _subsequent_ process (say, if we originally had

```
sf              -- :: State s (a -> b -> c)
  <*> sx        -- :: State s  a
    <*> sy      -- :: State s       b
```

and the leftmost `<*>` gave rise to a partially applied function in `State s` context) should have access to a state reflecting the impact of _both processes_, then we know that we want

```
sf (<*>) sx = State $ \s ->
  let (f, s' ) = runState sf $ s
      (x, s'') = runState sx $ s'
  in  (f x, s'')
```

Let's see how this plays out in our `filtering` example: because we know that `filtering setpred lst :: State (Set a) [a]`, it is really equivalent (but for the `runState` needed to "unpack" the function within) to a process `:: Set a -> ([a], Set a)`. Let's seed it with the empty set, which represents our total ignorance concerning what members the input `lst` could possibly have, and run the process on that seed:

```
runState (filtering setpred [1,2,1,3]) S.empty

runState 
  (foldr (\x kxs -> ...)
         (pure [])
         [1,2,1,3]) S.empty

runState
  (1 `(\x kxs -> ...)` $
    foldr (\x kxs -> ...) (pure []) [2,1,3]) S.empty

runState 
  (pure (\b y ys -> if b then y:ys else ys)
          <*> State (\set -> if 1 `S.member` set then (False, set) else (True, 1 `S.insert` set)) 
          <*> pure 1 
          <*> foldr ...) S.empty  
```

The way that we've defined `(<*>)` means that each term inside the `runState (...)` gets to handle the state `S.empty` once, in order from left-to-right: so evaluate `pure (\b y ys -> if b then y:ys else ys) <*> State ...` first: 

```
(pure (\b y ys -> if b then y:ys else ys)
  <*> State (\set -> if 1 `S.member` set then (False, set) else (True, 1 `S.insert` set)) 
  ) S.empty

let (f, s' ) = runState (pure (\b y ys -> ...)) s
    ...
```

The `(\b y ys -> ...)` is lifted into `State (Set a)` context, but doesn't perform any change of state on its own.

```
let (f, s' ) = (\b y ys -> ..., S.empty)
    (x, s'') = ...
```

`s' == S.empty`, which is now threaded through to `State (\set -> ...)`: since `runState . State == id`, this is now

```
let (f, s' ) = (\b y ys -> ..., S.empty)
    (x, s'') = (\set -> if 1 `S.member` set then (False, set) else (True, 1 `S.insert` set))
    ...
```

No element is a member of the empty set, so we go with the `else` branch.

```
let (f, s' ) = (\b y ys -> ..., S.empty)
    (x, s'') = (True, {1})
    ...
```

Haskell doesn't _really_ use `{`, `}` brackets as syntactic sugar for sets, but we'll do so to keep the pseudocode simple.

```
let (f, s' ) = (\b y ys -> ..., S.empty)
    (x, s'') = (True, {1})
in  (f x, s'')
```

What is `f x`? It's `(\b y ys -> if b then y:ys else ys) True`. The last element, `1`, wasn't in the set of elements seen thus far, so it's `True` that we want to keep this in the eventual result:

```
(\y ys -> y:ys, {1})
```

This pair contains _the thing we want to do with the current element `1`_ (include it in the eventual result), paired with a state to provide to the next contextualized term. We return to the overall `runState (...)` that we had begun working on:

```
let (f, s' ) = (\ y ys -> y:ys, {1})
    (x, s'') = runState (pure 1) $ s'
in  ...
```

But we're again faced with a term injected `pure`ly into applicative context - no change of state.

```
let (f, s' ) = (\ y ys -> y:ys, {1})
    (x, s'') = (1,              {1})
in  (f x, s'')

(\ys -> 1:ys, {1})
```

So after all that, we really turned out to have something along the lines of the following:

```
runState 
  (State (\set -> (\ys -> 1:ys, {1})) 
  <*> foldr (\x kxs -> ...) (pure []) [2,1,3]) S.empty  
```

Continuing to expand along these lines, we would find that our overall computation of `filtering setpred [1,2,1,3]` amounted to:

```
runState 
  (State (\set -> (\ys -> 1:ys, 1 `S.insert` set))          -- lst !! 0 == 1; not yet seen
  <*> State (\set -> (\ys -> 2:ys, 2 `S.insert` set))       -- lst !! 1 == 2; not yet seen
    <*> State (\set -> (\ys -> ys, set))                    -- lst !! 2 == 1; already seen as lst !! 0
      <*> State (\set -> (\ys -> 3:ys, 3 `S.insert` set))   -- lst !! 3 == 3; not yet seen
        <*> pure []) S.empty 
```

All that remains is to thread the initial, or seed, state `S.empty` through the overall computation: in this reduced form it is clear how the result is being built up (as the first pair element) while a state is being maintained, the state of values already seen that we don't wish to include again in the result (as the second pair element.) Running the process `State <*> ... <*> pure [] :: State (Set a) [a]` on `S.empty :: Set a` will result in a pair, from which only the first element will be of interest (_i.e._ the built-up sublist of distinct values.) This routinely will be the case: a process threading some seed state through a series of updates, which repeatedly consults the state in order to determine what steps should be taken in building up the result, will give rise to a pair from which just the first element is of interest. We abstract out the pattern of retrieving the final result of interest from a computation run on a seed state by defining `eval`:

```
eval :: State s a -> s -> a
eval mkState = fst . runState mkState
```

With this mode of use for `State s a` terms made conveniently available to us as `eval`, we have the solution to the former, immediately evident problem of how to "extract" the distinct list that we've done all this work for:

```
distinct :: Ord a => [a] -> [a]
distinct lst = 
  let runFiltering = filtering        -- :: (a -> State (Set a) Bool) -> [a] -> State (Set a) [a]
                      setpred         -- :: (a -> State (Set a) Bool)
                      lst             -- ::                              [a]
  in  eval runFiltering S.empty
```


***

<sup id="fn1">1. (which would be impossible in any case because, given any state <code>t</code> which has been arrived at in the course of the <code>let ...</code>-portion of the function body, we could choose instead of returning a pair <code>(f x, t)</code> to return the pair <code>(f x, runState q1 $ t)</code> where <code>q1</code> is either of the inputs to <code>(&lt; * &gt;)</code>, or <code>(f x, runState q2 $ runState q1 $ t)</code>, or so on <em>ad infinitum</em>)<a href="#ref1" title="Jump back to footnote 1">↩</a>
</sup>
<br>
<br>
<sup id="fn2">2. This is a simplified account of the <code>State s</code> Applicative's implementation: as the following, via <code>ghci</code>, makes clear:<br>
<pre>
<code>
> import Control.Monad.State
> :info State
type State s = StateT s Data.Functor.Identity.Identity :: * -> *
    -- Defined in ‘Control.Monad.Trans.State.Lazy’
> :info StateT
type role StateT nominal representational nominal
newtype StateT s (m :: * -> *) a
  = StateT {runStateT :: s -> m (a, s)}
    -- Defined in ‘Control.Monad.Trans.State.Lazy’
instance [safe] (Functor m, Monad m) => Applicative (StateT s m)
  -- Defined in ‘Control.Monad.Trans.State.Lazy’
  [...]
</code>
</pre><br>the type constructor <code>State</code> is "officially" implemented in terms of the higher-order type constructor <code>StateT</code> (in particuar, as the type constructor due to instantiating <code>StateT</code> with the <code>Identity</code> functor, which is also a monad.) See <a href="https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.State.Lazy.html#line-204">here</a> for the relevant <code>(<*>)</code> implementation.<a href="#ref2" title="Jump back to footnote 2">↩</a></sup>
<br>
<sup id="fn3">3. In reality, maintaining a list of values encountered would accomplish the same thing, so long as we took care to check each element, as we encountered it, for membership in the list so far, and refrain from adding it again if present: but this search is faster on a set (<em>O(log n)</em>) vs. a list (<em>O(n)</em>) in general, and furthermore a set is mathematically speaking the appropriate way to represent a collection of values in which each occurs at most once.<a href="#ref3" title="Jump back to footnote 3">↩</a>
<br>
<sup id="fn4">4. It is <em>not true in general</em> that, given a value of type <code>f a</code>, we can hope to extract an <code>a</code> from it. For instance, when <code>f := Maybe</code>, we can't perform such an extraction safely because not every data constructor for the type <code>Maybe a</code> <em>witnesses</em> the type variable <em>a</em> (in particular, <em>Nothing</em> the nullary data constructor is the culprit here.) However, in the case of what is essentially a function type, <code>State s [a] ~ (s -> ([a], s))</code>, we hold out hope that an <code>[a]</code> can be obtained so long as we apply the term to the appropriate <code>s</code>.<a href="#ref4" title="Jump back to footnote 4">↩</a>
<br>
<sup id="fn5">5. This corresponds to 1c. in the listing out of applicatives which we considered at the start. 2c., the other lawful implementation, corresponds to threading state through the two processes in the <em>opposite</em> order, <code>sx</code> then <code>sf</code>.<a href="#ref5" title="Jump back to footnote 5">↩</a>
