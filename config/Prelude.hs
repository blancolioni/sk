module Prelude where

class Eq a where
   (==), (/=) :: a -> a -> Bool
   x == y = not (x /= y)
   x /= y = not (x == y)

not :: Bool -> Bool
not True = False
not False = True
