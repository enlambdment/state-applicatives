module Examples where 

import qualified Data.Set as S

import StateApplicatives

-- This code is adapted from the System F fp-course:
-- https://github.com/system-f/fp-course

-- 'filtering' generalizes 'filter' for use with a predicate
-- whose boolean output inhabits a context characterized by
-- the Applicative k. Compare:

-- filter :: (a ->   Bool) -> [a] ->   [a]
filtering :: Applicative k =>
             (a -> k Bool) -> [a] -> k [a]
filtering kpred lst =
  foldr (\x kxs -> pure (\b y ys -> if b then y : ys else ys)
                   <*> kpred x 
                   <*> pure x 
                   <*> kxs)
        (pure [])
        lst

-- 'distinct' leverages the "folding-right inside an applicative
-- context" activity of 'filtering' to build up a list of distinct
-- elements inside a *stateful* context which keeps track of elements
-- :: a seen thus far; then the final result is taken from the overall 
-- process's output pair.
distinct :: Ord a => [a] -> [a]
distinct xs = 
  let compute = filtering (\x -> State $ 
                            \s -> if x `S.member` s 
                                    then (False, s)
                                    else (True, x `S.insert` s))
                          xs 
  in  eval compute S.empty
