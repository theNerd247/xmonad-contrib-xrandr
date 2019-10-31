-- TODO: rename this data type using a more category theory way of looking at
-- the world
module Data.PreList 
  ( PreList (..)
  , unPrelist
  )
where

import Data.Foldable
import Control.Monad ((<=<))
import Control.Applicative

-- | Encodes the idea of suspended function application. --
-- For example:
--
--   unPrelist $ EndoVal [1] <> EndoF (2:) == [2,1] == unPrelist $ EndoF (2:[]) <> EndoVal [1]
data PreList a = 
    EndoVal a
  | EndoF (a -> a)

-- | The Semigroup instance of PreList encodes the idea of either applying a
-- function to the current result or composing functions in the correct order.
-- This is useful for building lists from a Foldable where the elements in the
-- resulting list could be in a different order.
-- | only the left most result is kept. @EndoVal x <> EndVal y == EndoVal x@
instance Semigroup (PreList a) where
   (EndoVal s) <> (EndoVal _) = EndoVal s  
   (EndoF f)   <> (EndoF g)   = EndoF $ g . f
   (EndoVal s) <> (EndoF f)   = EndoVal $ f s
   (EndoF f)   <> (EndoVal s) = EndoVal $ f s

-- | @mempty = EndoF id@
instance Monoid (PreList a) where
  mempty = EndoF id

-- | Extract the currently evaluated value or fail with @empty@
unPrelist :: (Alternative f) => PreList a -> f a
unPrelist (EndoVal a) = pure a
unPrelist (EndoF _)   = empty

applyPrelist :: (Alternative f, Monad f, Foldable t) => f (t (PreList a)) -> f a
applyPrelist = unPrelist <=< fmap fold
