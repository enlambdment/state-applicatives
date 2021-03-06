{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE FlexibleInstances  #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module StateApplicatives where

import Control.Applicative ( liftA2 )
import Text.Show.Functions 

newtype State s a = State
  { runState :: s -> (a, s) }

-- Is this "evil" since it defines equality of terms
-- as mere equality over the specific input string ""?
-- (It seems to make the property testing work...)
instance Eq a => Eq (State String a) where
  st == st' = 
    let p  = runState st  $ ""
        p' = runState st' $ ""
    in  p == p'

-- we won't set any type constraints requiring Show s, Show a 
-- because 'instance Show (State s a)' won't be used except to
-- get rid of e.g.
--  "No instance for (Show (State Char Int))
--       arising from a use of ‘quickCheck’"
instance Show (State s a) where
  show st = "<placeholder>"

exec :: State s a -> s -> s
exec mkState = snd . runState mkState

eval :: State s a -> s -> a
eval mkState = fst . runState mkState

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put t = State $ \_ -> ((), t)

instance Functor (State s) where
  fmap :: (a -> b)
          -> State s a
          -> State s b
  f `fmap` st = State $
    \x -> let (y,   t) = runState st $ x
          in  (f y, t)

instance Applicative (State s) where 
  pure  :: a 
           -> State s a 
  -- we can use the Applicative of functions
  pure x = State $ liftA2 (,) (const x) id
  (<*>) :: State s (a -> b)
           -> State s a 
           -> State s b
  sf <*> sx = State $ \s ->

  --1. Use sf first, then sx on what results
    let (f, s' ) = runState sf $ s
        (x, s'') = runState sx $ s'
    in  (f x, 
          -- s)    -- a.
          -- s')   -- b.
          s'')  -- c.   <-- This is the *canonical implementation for 'Applicative (State s)'.*

  --2. Use sx first, then sf on what results
    -- let (x, s' ) = runState sx $ s
    --     (f, s'') = runState sf $ s'
    -- in  (f x, 
          -- s)    -- a.
          -- s')   -- b.
          -- s'')  -- c.

  --3. Use sf, sx at the same time
    -- let (f, s1') = runState sf $ s 
    --     (x, s2') = runState sx $ s 
    -- in  (f x,
          -- s)    -- a.
          -- s1')  -- b.
          -- s2')  -- c.

{- Results, checking instance laws:

1a. Fails identity, composition,              functor
1b. Fails identity, composition, interchange, functor
1c. Passes

2a. Fails identity, composition,              functor
2b. Fails           composition, interchange
2c. Passes

3a. Fails identity,                           functor
3b. Fails identity, composition, interchange, functor
3c. Fails                        interchange

-}















{-

  -- sf (<*>) sx = undefined

  -- 1. 
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, y) = (runState sf $ x)      -- (a -> b, s)
  --         (a, _) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, snd $ runState sx $ y)

  -- applicative:
  --   identity:     +++ OK, passed 500 tests.
  --   composition:  +++ OK, passed 500 tests.
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  +++ OK, passed 500 tests.
  --   functor:      +++ OK, passed 500 tests.


  -- 2.
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, _) = (runState sf $ x)      -- (a -> b, s)
  --         (a, y) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, snd $ runState sf $ y)

  -- applicative:
  --   identity:     +++ OK, passed 500 tests.
  --   composition:  +++ OK, passed 500 tests.
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  +++ OK, passed 500 tests.
  --   functor:      +++ OK, passed 500 tests.


  -- 3.
  sf <*> sx = 
    State $ \x ->
      let (f, x1) = (runState sf $ x)       -- (a -> b, s)
          (a, x2) = (runState sx $ x1)      -- (a,      s)
      in  (f a, x2)

  -- applicative:
  --    identity:     +++ OK, passed 500 tests.
  --    composition:  +++ OK, passed 500 tests.
  --    homomorphism: +++ OK, passed 500 tests.
  --    interchange:  +++ OK, passed 500 tests.
  --    functor:      +++ OK, passed 500 tests.


  -- 4.
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (a, x1) = (runState sx $ x)      -- (a,      s)
  --         (f, x2) = (runState sf $ x1)     -- (a -> b, s)  
  --     in  (f a, x2)

  -- applicative:
  --    identity:     +++ OK, passed 500 tests.
  --    composition:  +++ OK, passed 500 tests.
  --    homomorphism: +++ OK, passed 500 tests.
  --    interchange:  +++ OK, passed 500 tests.
  --    functor:      +++ OK, passed 500 tests.



  -- And now for some less principled Applicative instances:
  -- 5. 
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, y) = (runState sf $ x)      -- (a -> b, s)
  --         (a, _) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, y)

  -- applicative:
  --   identity:     *** Failed! Falsified (after 3 tests):  
  -- <placeholder>
  --   composition:  *** Failed! Falsified (after 2 tests):  
  -- <placeholder>
  -- <placeholder>
  -- <placeholder>
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  *** Failed! Falsified (after 2 tests):  
  -- <placeholder>
  -- ""
  --   functor:      *** Failed! Falsified (after 2 tests):  
  -- <function>
  -- <placeholder>


  -- 6.
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, _) = (runState sf $ x)      -- (a -> b, s)
  --         (a, y) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, y)

  -- applicative:
  --   identity:     +++ OK, passed 500 tests.
  --   composition:  +++ OK, passed 500 tests.
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  *** Failed! Falsified (after 3 tests and 1 shrink):  
  -- <placeholder>
  -- ""
  --   functor:      +++ OK, passed 500 tests.

  -- 7.
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, _) = (runState sf $ x)      -- (a -> b, s)
  --         (a, y) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, snd $ runState sx $ y)

  -- applicative:
  --   identity:     *** Failed! Falsified (after 2 tests):  
  -- <placeholder>
  --   composition:  *** Failed! Falsified (after 4 tests):  
  -- <placeholder>
  -- <placeholder>
  -- <placeholder>
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  *** Failed! Falsified (after 4 tests and 1 shrink):  
  -- <placeholder>
  -- ""
  --   functor:      *** Failed! Falsified (after 2 tests):  
  -- <function>
  -- <placeholder>

  -- 8.
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, y) = (runState sf $ x)      -- (a -> b, s)
  --         (a, _) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, snd $ runState sf $ y)

  -- applicative:
  --   identity:     *** Failed! Falsified (after 2 tests):  
  -- <placeholder>
  --   composition:  *** Failed! Falsified (after 5 tests):  
  -- <placeholder>
  -- <placeholder>
  -- <placeholder>
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  *** Failed! Falsified (after 4 tests):  
  -- <placeholder>
  -- ""
  --   functor:      *** Failed! Falsified (after 3 tests):  
  -- <function>
  -- <placeholder>

  -- 9.
  -- sf <*> sx = 
  --   State $ \x ->
  --     let (f, _) = (runState sf $ x)      -- (a -> b, s)
  --         (a, _) = (runState sx $ x)      -- (a,      s)
  --     in  (f a, x)

  -- applicative:
  --   identity:     *** Failed! Falsified (after 2 tests):  
  -- <placeholder>
  --   composition:  +++ OK, passed 500 tests.
  --   homomorphism: +++ OK, passed 500 tests.
  --   interchange:  +++ OK, passed 500 tests.
  --   functor:      *** Failed! Falsified (after 3 tests):  
  -- <function>
  -- <placeholder>

-}
