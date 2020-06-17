{-# LANGUAGE FlexibleInstances  #-}

module Main where 

import Control.Monad (liftM, liftM2, liftM3)
 -- 'Gen' is a *monad*, so we need liftM / liftM2
 -- to lift data constructors into Gen constructors
 -- for the data-type
import Data.Monoid (Product, Sum)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import StateApplicatives

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

type SIC = (String, Int, Char)

main :: IO ()
main = do 
  quickBatch $ applicative (undefined :: State String SIC)
