# state-applicatives

_The original impetus for this discussion, and the code for `filtering`, `foldRight`, `distinct` etc., were due to working through the System F functional programming course in Haskell, [fp-course](https://github.com/system-f/fp-course)_

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

## Counting possible implementations

There are various ways of writing an implementation for `(<*>)` that will type-check. Instead of exhaustively listing out these possibilities,<sup><a href="#fn1" id="ref1">1</a></sup> we may limit ourselves to considering just those implementations which use each of `sf`, `sx` exactly once, to produce terms `:: a -> b`, `:: a` respectively (needed for the `:: b` value) as well as produce states (which we have to decide what to do with.) Our rough argument in favor of considering _only_ the implementations which use each of `sf`, `sx` exactly once could be seen as an appeal to symmetry: we wouldn't want the operator `(<*>)` to "prefer" either input over the other for no clear reason, unless we later came across a reason to do otherwise. Both inputs `sf`, `sx` should participate in the overall process to an equal extent.

With this principle in mind, we could list out the possibilities as follows: 

1. Use `sf` first, then `sx` on the resulting state. In the state part of the output pair, use 
  a. `s`
  b. `exec sf $ s`
  c. `exec sx $ exec sf $ s`
2. Use `sx` first, then `sf` on the resulting state. In the state part of the output pair, use
  a. `s`
  b. `exec sx $ s`
  c. `exec sf $ exec sx $ s`
3. Use `sf`, `sx` both at the same time.In the state part of the output pair, use
  a. `s`
  b. `exec sf $ s`
  c. `exec sx $ s`

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
filter :: (a ->   Bool ) -> [a] ->   [a]
```

TODO

***

<sup id="fn1">1. (which would be impossible in any case because, given any state <code>t</code> which has been arrived at in the course of the <code>let ...</code>-portion of the function body, we could choose instead of returning a pair <code>(f x, t)</code> to return the pair <code>(f x, runState q1 $ t)</code> where <code>q1</code> is either of the inputs to <code>(<*>)</code>, or <code>(f x, runState q2 $ runState q1 $ t)</code>, or so on <em>ad infinitum</em>)<a href="#ref1" title="Jump back to footnote 1">↩</a></sup>

<sup id="fn2">2. This is a simplified account of the <code>State s</code> Applicative's implementation: as the following, via <code>ghci</code>, makes clear:

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

the type constructor <code>State</code> is "officially" implemented in terms of the higher-order type constructor <code>StateT</code> (in particuar, as the type constructor due to instantiating <code>StateT</code> with the <code>Identity</code> functor, which is also a monad.) See [here](https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.State.Lazy.html#line-204) for the relevant <code>(<*>)</code> implementation.<a href="#ref2" title="Jump back to footnote 2">↩</a></sup>
