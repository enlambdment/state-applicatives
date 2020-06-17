# state-applicatives

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

## Summary

Given a term `st :: State s a` and a term `x :: s` to apply its "unpacked" function `runState st` to, there is exactly one "result state" i.e. `exec st x` which results.

Considering, then, the use of `<*>` on two terms 

```
sf :: State s (a -> b)
sx :: State s a
```

each must be run on an input `x :: s` in order to get terms `eval sf x :: a -> b`, `eval sx x :: a` that can be used to arrive at the `b`-part of the output pair `(b, s)`. This process gives rise to two terms 

```
y := exec sf x :: s
z := exec sx x :: s
```

and we can either 

1. use _neither_ of `y`, `z` i.e. simply give back the original `x` as the resulting state of `exec (sf <*> sx) x`, unchanged; running `sf` on it; or running `sx` on it. 3 ways in total to arrive at a putative term for the value of `exec (sf <*> sx) x` this way;
2. use `y` - either giving it back unchanged, etc. 3 ways in total, etc.
3. or use `z` - either giving it back unchanged, etc. 3 ways in total, etc.

This results in 3 * 3 = 9 different possible implementations for this Applicative instance's `(<*>)` that will _typecheck_; of these, 4 will pass property testing which makes use of the `applicative` test batch offered by the _checkers_ library.

These lawful implementations have in common that _both `sf`, `sx` participate in the computation of the final state, `exec (sf <*> sx) x`, that results_; however, they differ with respect to the _order_ in which they are applied to `x`, and in whether or not any intermediate state is used to produce either or both of the terms `:: a -> b`, `:: a`.
