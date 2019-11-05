{-# LANGUAGE OverloadedStrings #-}

module Xrandr.Types
  ( Position (..)
  , Rotation (..)
  , OutputName (..)
  , Config (..)
  , Mode (..)
  , Modes
  , ScreenCmd
  , Screens
  , disableSecondary
  , allScreensLeft
  , allScreensRight
  , nextScreenRotation
  , prevScreenRotation
  , nextScreenPosition
  , prevScreenPosition
  , primary
  , secondary
  , disabled
  , disconnected
  , configWithNormalRotation
  , autoEnable
  , modifyConfig
  , modifyPosition
  , modifyRotation
  , modifyRotationOfScreen
  , toScreens
  , fromScreens
  )
where

import Control.Arrow
import Xrandr.Types.Internal

configWithNormalRotation :: Modes -> Config
configWithNormalRotation = Config Normal

primary a b       = toScreens $ Primary a b
secondary a b c d = toScreens $ Secondary a b c d
disabled a b c    = toScreens $ Disabled a b c
disconnected a b  = toScreens $ Disconnected a b

allScreensLeft :: ScreenCmd a
allScreensLeft = modifyPosition $ const LeftOf

allScreensRight :: ScreenCmd a
allScreensRight = modifyPosition $ const RightOf

nextScreenRotation :: OutputName -> ScreenCmd a
nextScreenRotation n = modifyRotationOfScreen n succ

prevScreenRotation :: OutputName -> ScreenCmd a
prevScreenRotation n = modifyRotationOfScreen n pred

autoEnable :: ScreenCmd a
autoEnable (Disabled n c s) = Secondary n c LeftOf s
autoEnable x = x

disableSecondary :: ScreenCmd a
disableSecondary (Secondary n c _ s) = Disabled n c s
disableSecondary x = x

modifyRotationOfScreen :: OutputName -> (Rotation -> Rotation) -> ScreenCmd a
modifyRotationOfScreen n = modifyScreenAt n . modifyRotation

modifyRotation :: (Rotation -> Rotation) -> ScreenCmd a
modifyRotation f = modifyConfig $ \c -> c { rotation = f $ rotation c }

modifyConfig :: (Config -> Config) -> ScreenCmd a
modifyConfig = onSecondary . first

nextScreenPosition :: ScreenCmd a
nextScreenPosition = modifyPosition succ

prevScreenPosition :: ScreenCmd a
prevScreenPosition = modifyPosition pred

modifyPosition :: (Position -> Position) -> ScreenCmd a
modifyPosition = onSecondary . second

onSecondary :: ((Config, Position) -> (Config, Position)) -> ScreenCmd a
onSecondary f (Secondary n c p s) = let (c', p') = f (c,p) in Secondary n c' p' s
onSecondary _ x = x

modifyScreenAt :: OutputName -> ScreenCmd a -> ScreenCmd a
modifyScreenAt name f x
  | getOutputName x == name = f x
  | otherwise               = x

getOutputName :: ScreenF a -> OutputName
getOutputName (Primary n _)       = n
getOutputName (Secondary n _ _ _) = n
getOutputName (Disabled n _ _)    = n
getOutputName (Disconnected n _)  = n
