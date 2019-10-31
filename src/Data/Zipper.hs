module Xrandr.Zipper 
  ( Zipper (..)
  , left
  , right
  )
where

data Zipper a = Zipper
  { focus :: a
  , next :: [a]
  , prev :: [a]
  } deriving (Show, Eq, Ord, Functor)

left :: Zipper a -> Zipper a
left x@(Zipper _ _ []) = x
left (Zipper n ns (p:ps)) = Zipper p (n:ns) ps

right :: Zipper a -> Zipper a
right x@(Zipper _ [] _) = x
right (Zipper p (n:ns) ps) = Zipper n ns (p:ps)


instance (ToCmd a) => ToCmd (Zipper a) where
  buildCmd = buildCmd . focus
