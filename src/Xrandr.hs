{-# LANGUAGE OverloadedStrings #-}

module Xrandr where

import Control.Monad ((>=>))
import Shelly (shelly, run, run_)
import qualified Data.Text as T

import Xrandr.Types
import Xrandr.Parser

-- | Requirements
--
--  * Screens can be either: Connected or Disconnected
--  * Connected Screens can be Enabled or Disabled
--  * Enabled screens must have a Mode and Rotation defined
--  * Default rotation is Normal which means the screen is upright).
--  * Only 1 screen can be the Primary, and all the others must be Secondary
--  * Disabled / Disconnected screens must not have a position
--  * Connected Screens must have a position relative to another screen
--      Secondary -> Primary | Secondary; Primary doesn't have a position
--
--  * Disabled and Disconnected screens should coorespond to the command
--    `--output <OutputName> --off`
--  * We would like to keep the Disabled information for later. When the screen
--    state expression is used in a State monad this will allow for disabling
--    secondary screens that are currently enabled.
--  * 

-- | represents the physical layout of those screens which are connected
spawnXrandr :: (Screens -> Screens) -> IO ()
spawnXrandr f = 
  readCurrentScreens >>= either print (runXrandrCmdWith f) 

readCurrentScreens :: IO (Either String Screens)
readCurrentScreens = parseScreens <$> (runXrandr [])

runXrandrCmdWith :: (Screens -> Screens) -> Screens -> IO ()
runXrandrCmdWith f = runXrandr . makeCmd . f >=> const (return ())
 
runXrandr :: Cmd -> IO T.Text
runXrandr = shelly . run "xrandr"
