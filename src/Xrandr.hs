{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Xrandr where

import Control.Applicative
import Control.Arrow
import Control.Monad ((>=>))

import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
import Data.Functor.Foldable
import Data.List (partition, find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Control.Monad
import Data.String
import Numeric.Natural
import Shelly (shelly, run, run_)
import qualified Data.Text as T

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
modeX = fst . modeName

modeY = snd . modeName

primary a b = Fix $ Primary a b
secondary a b c d = Fix $ Secondary a b c d
disabled a b c = Fix $ Disabled a b c
disconnected a b = Fix $ Disconnected a b

autoEnable :: Screens -> Screens
autoEnable = cata autoEnable'

autoEnable' :: ScreenF Screens -> Screens
autoEnable' (Disabled n c s) = secondary n c LeftOf s
autoEnable' x = Fix x

setSecondaryPositions :: Position -> Screens -> Screens
setSecondaryPositions = cata . setSecondaryPositions'

setSecondaryPositions' :: Position -> ScreenF Screens -> Screens
setSecondaryPositions' p (Secondary n c _ s) = secondary n c p s
setSecondaryPositions' _ x = Fix x

allScreensOff :: Screens -> Screens
allScreensOff = cata allScreensOff'

allScreensOff' :: ScreenF Screens -> Screens
allScreensOff' (Secondary n c _ s) = disabled n c s
allScreensOff' x = Fix x

allScreensLeft :: Screens -> Screens
allScreensLeft = setSecondaryPositions LeftOf

allScreensRight :: Screens -> Screens
allScreensRight = setSecondaryPositions RightOf

makeCmd :: Screens -> Cmd
makeCmd = para buildCmd'

buildCmd' :: ScreenF (Screens, Cmd) -> Cmd
buildCmd' (Primary n c)                     =         (buildCmd n) <> (buildCmd c) <> ["--primary"]
buildCmd' (Secondary n c p (screens, cmds)) = cmds <> (buildCmd n) <> (buildCmd c) <> [positionArg p, name $ nextOutputName screens]
buildCmd' (Disabled n _ (_, cmds))          = cmds <> (buildCmd n)                 <> ["--off"]
buildCmd' (Disconnected n (_, cmds))        = cmds <> (buildCmd n)                 <> ["--off"]

positionArg :: Position -> T.Text
positionArg LeftOf  = "--left-of"
positionArg RightOf = "--right-of"
positionArg Above   = "--above"
positionArg Below   = "--below"
positionArg SameAs  = "--same-as"

nextOutputName :: Screens -> OutputName
nextOutputName = cata nextOutputName'

nextOutputName' :: ScreenF OutputName -> OutputName
nextOutputName' (Primary n _)      = n
nextOutputName' (Secondary n _ _ _)  = n
nextOutputName' (Disabled _ _ n)   = n
nextOutputName' (Disconnected _ n) = n

configWithNormalRotation :: Modes -> Config
configWithNormalRotation = Config Normal

spawnXrandr :: (Screens -> Screens) -> IO ()
spawnXrandr f = 
  readCurrentScreens >>= either print (runXrandrCmdWith f) 

readCurrentScreens :: IO (Either String Screens)
readCurrentScreens = parseScreens <$> (runXrandr [])

runXrandrCmdWith :: (Screens -> Screens) -> Screens -> IO ()
runXrandrCmdWith f = runXrandr . makeCmd . f >=> const (return ())
 
runXrandr :: [T.Text] -> IO T.Text
runXrandr = shelly . run "xrandr"
