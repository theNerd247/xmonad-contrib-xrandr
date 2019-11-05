{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExplicitForAll#-}
{-# LANGUAGE RankNTypes #-}



module Xrandr 
  ( runXrandr
  , readCurrentScreens
  , xrandr
  , xrandr_
  , module Xrandr.Types
  )
where

import Control.Monad ((>=>))
import Shelly (shelly, run, run_)
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Fail
import Control.Arrow

import Xrandr.Cmd
import Xrandr.Types
import Xrandr.Parser

runXrandr :: (forall a. ScreenCmd a) -> Screens -> (Screens, Cmd)
runXrandr f = 
    (second snd) 
  . (fromScreens $ bothAlg (toScreens . f) (makeCmd . f))

readCurrentScreens :: IO (Either String Screens)
readCurrentScreens = parseScreens <$> (xrandr [])

bothAlg :: (Functor f) => (f a -> a) -> (f b -> b) -> f (a,b) -> (a,b)
bothAlg f g = (f . fmap fst) &&& (g . fmap snd)

xrandr_ :: Cmd -> IO ()
xrandr_ c = xrandr c >> return ()
 
xrandr :: Cmd -> IO T.Text
xrandr = shelly . run "xrandr"
