module Data.EnumCycle 
  ( next
  , pre
  , nextCycle
  )
where

next :: (Bounded a, Enum a) => a -> a
next = nextCycle (+1) 

pre :: (Bounded a, Enum a) => a -> a
pre = nextCycle (\x -> x - 1)

nextCycle :: (Bounded a, Enum a) => (Int -> Int) -> a -> a
nextCycle f x = 
  let 
    m = fromEnum . minb' $ x
    l = (\n -> 1 + n - m) . fromEnum . maxb' $ x
    x' = f $ fromEnum x
  in toEnum $ (x' `mod` l) + m

minb' :: (Bounded a) => a -> a
minb' = const minBound

maxb' :: (Bounded a) => a -> a
maxb' = const maxBound
