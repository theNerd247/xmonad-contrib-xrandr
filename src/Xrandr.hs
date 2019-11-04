{-# LANGUAGE OverloadedStrings #-}

module Xrandr 
  ( runXrandr
  , module Xrandr.Types
  )
where

import Control.Monad ((>=>))
import Shelly (shelly, run, run_)
import qualified Data.Text as T
import Control.Monad.IO.Class

import Xrandr.Cmd
import Xrandr.Types
import Xrandr.Parser

runXrandr :: (MonadIO m) => ScreenCmd (OutputName, Cmd) -> m ()
runXrandr = liftIO 
  . (readCurrentScreens >>=) 
  . either print 
  . ((xrandr_ . snd).) 
  . fromScreens 
  . (makeCmd.)

readCurrentScreens :: IO (Either String Screens)
readCurrentScreens = parseScreens <$> (xrandr [])

xrandr_ :: Cmd -> IO ()
xrandr_ c = xrandr c >> return ()
 
xrandr :: Cmd -> IO T.Text
xrandr = shelly . run "xrandr"
