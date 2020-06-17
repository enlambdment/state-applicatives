{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Monad            (liftM, liftM2, liftM3)
 -- 'Gen' is a *monad*, so we need liftM / liftM2
 -- to lift data constructors into Gen constructors
 -- for the data-type
import           Data.Monoid              (Product, Sum)
import           StateApplicatives
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Function

-- -- need the following for Checkers to be usable
-- instance (Eq s, Eq a) => EqProp (State s a) where
--   (=-=) = eq -- does this suffice?

instance Eq a => EqProp (State String a) where
  (=-=) = eq

-- recall:
-- State s a ~ s -> (a, s)
instance (CoArbitrary s, Arbitrary s, Arbitrary a) =>
  Arbitrary (State s a) where
    -- arbitrary :: Arbitrary a => Gen a
    -- This should define a way to create _ :: Gen (State s a)
    -- by leveraging:
    --  coarbitrary :: s -> Gen q -> Gen q
    --  arbitrary   :: Gen s
    --  arbitrary   :: Gen a
    arbitrary = liftM State arbitrary

-- The 'applicative' test batch is given to us, by the Checkers library
-- However, we need to pass 'applicative' a dummy value that will get
-- used for its type:

  -- applicative ::
  -- (Applicative m, CoArbitrary a,
  -- Arbitrary a, Arbitrary b, ... Show a, ...) =>
  -- m (a, b, c) -> TestBatch



-- Rather than using "applicative" built in to checkers
-- to confirm the applicative laws over randomly generated terms
--    p :: State s a,   x :: s
-- we can define the relevant properties ourselves & then
-- check pointwise equality of the functions which left-hand and right-hand
-- sides of the applicative laws give rise to.

-- functor laws (every applicative must also be a lawful functor)
stateFunctorId :: (Eq s, Eq a) =>
                  s -> State s a -> Bool
stateFunctorId x st =
  let lhs = id `fmap` st
      rhs = st
  in  (runState lhs) x == (runState rhs) x

stateFunctorComp :: (Eq s, Eq c) =>
                    s -> Fun a b            -- ~ (a -> b), sort of
                      -> Fun b c            -- ~ (b -> c), sort of
                      -> State s a -> Bool
stateFunctorComp x (Fun _ f) (Fun _ g) st =
  let lhs = (g . f) <$> st
      rhs = g <$> (f <$> st)
  in  (runState lhs) x == (runState rhs) x

-- applicative laws
stateApplicativeId :: (Eq s, Eq a) =>
                      s -> State s a -> Bool
stateApplicativeId x st =
  let lhs = pure id <*> st
      rhs = st
  in  (runState lhs) x == (runState rhs) x

stateApplicativeComp :: (Eq s, Eq c) =>
                        s -> State s a
                          -> State s (a -> b)
                          -> State s (b -> c) -> Bool 
stateApplicativeComp x st_a st_f st_g = 
  let lhs = pure (.) <*> st_g <*> st_f <*> st_a 
      rhs = st_g <*> (st_f <*> st_a)
  in  (runState lhs) x == (runState rhs) x

stateApplicativeHom :: (Eq s, Eq b) =>
                       s -> Fun a b 
                         -> a
                         -> Bool
stateApplicativeHom x (Fun _ f) a =
  let lhs = (pure f) <*> (pure a)
      rhs = pure (f a)
  in  (runState lhs) x == (runState rhs) x

stateApplicativeIntrchg :: (Eq s, Eq b) =>
                           s -> (State s (a -> b))
                             -> a 
                             -> Bool
stateApplicativeIntrchg x st a =
  let lhs = st <*> pure a 
      rhs = pure ($ a) <*> st 
  in  (runState lhs) x == (runState rhs) x

-- we *also* should check that the definition provided for <*>
-- is compatible with the instance of Functor for the same type:
-- as expressed in Test.QuickCheck.Classes exported by checkers:

--  functorP f x       = (fmap f x) =-= (pure f <*> x)

stateApplicativeFunctor :: (Eq s, Eq b) =>
                           s -> Fun a b
                             -> State s a
                             -> Bool
stateApplicativeFunctor x (Fun _ f) st = 
  let lhs = f `fmap` st 
      rhs = (pure f) <*> st 
  in  (runState lhs) x == (runState rhs) x

-- type synonyms
type SIC = (String, Int, Char)
type IntToStr = Fun Int String
type StrToChar = Fun String Char

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: State String SIC)
  -- trying pointwise-equality-based quickcheck tests on Functor (State String)
  quickCheck $ (stateFunctorId          :: Char -> State Char Int                          -> Bool)
  quickCheck $ (stateFunctorComp        :: Char -> IntToStr -> StrToChar -> State Char Int -> Bool)
  quickCheck $ (stateApplicativeId      :: Char -> State Char Int                          -> Bool)
  quickCheck $ (stateApplicativeComp    :: Char -> State Char Int 
                                                -> State Char (Int -> String)
                                                -> State Char (String -> Char) -> Bool)
  quickCheck $ (stateApplicativeHom     :: Char -> IntToStr -> Int -> Bool)
  quickCheck $ (stateApplicativeIntrchg :: Char -> State Char (Int -> String)
                                                -> Int 
                                                -> Bool)
  quickCheck $ (stateApplicativeFunctor :: Char -> IntToStr -> State Char Int -> Bool)
