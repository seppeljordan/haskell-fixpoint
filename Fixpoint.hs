module Fixpoint (fixpoint, root, derive)
where


epsilon :: Float
epsilon = 0.0001

fixpoint :: (Float -> Float) -> Float
fixpoint f = root (\x -> (f x) - x)

derive :: (Float -> Float) -> (Float -> Float)
derive f = \x -> (f (x+epsilon) - f x) / epsilon

newton :: (Float -> Float) -> Float -> Float
newton f x0 = let aux f x = if abs (f x) < epsilon
                         then x
                         else aux f (new x)
                  new x = x - (f x) / (derive f x)
           in aux f x0

root = \x -> newton x 1
