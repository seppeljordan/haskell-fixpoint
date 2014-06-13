module Fixpoint (fixpoint, root, derive)
-- This module implements a simple fixpoint procedure for (Float ->
-- Float)-Procedures.  The standard accuracy is 0.0001 for fixpoint,
-- root and derive procedures
where

-- This is the accuracy for our calculations
epsilon :: Float
epsilon = 0.0001

-- Compute a fixpoint of a (Float -> Float) procedure
fixpoint :: (Float -> Float) -> Float
fixpoint f = root (\x -> (f x) - x)

-- Compute the derivate of a (Float -> Float) procedure
derive :: (Float -> Float) -> (Float -> Float)
derive f = \x -> (f (x+epsilon) - f x) / epsilon

-- Compute a root of a (Float -> Float) procedure, e.g. f x == 0.
-- This procedure uses the new algorithm.
newton :: (Float -> Float) -> Float -> Float
newton f x0 = let aux f x = if abs (f x) < epsilon
                         then x
                         else aux f (new x)
                  new x = x - (f x) / (derive f x)
           in aux f x0

-- Compute one root a (Float -> Float) procedure, e.g. f x == 0.
root = \x -> newton x 1
