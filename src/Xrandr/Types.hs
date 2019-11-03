{-# LANGUAGE OverloadedStrings #-}

module Xrandr.Types
  ( 
  -- | primitives for screens
    primary
  , secondary
  , disabled
  , disconnected
  , Position (..)
  , Rotation (..)
  , OutputName (..)
  , Config (..)
  , Mode (..)
  , Modes
  , Cmd
  , Screens
  , disableSecondary
  , allSecondaryOff
  , allScreensLeft
  , allScreensRight
  , nextScreenRotation
  , prevScreenRotation
  , configWithNormalRotation
  , makeCmd
  , primary
  , secondary
  , disabled
  , disconnected
  -- | default config
  , configWithNormalRotation
  -- | screens combinators
  , autoEnable
  , modifyConfig
  , modifyPosition
  , modifyRotation
  , modifyRotationOfScreen
  , disableSecondary
  , makeCmd
  , onAllScreens
  )
where

import Xrandr.Types.Internal

configWithNormalRotation :: Modes -> Config
configWithNormalRotation = Config Normal

primary a b       = toScreens $ Primary a b
secondary a b c d = toScreens $ Secondary a b c d
disabled a b c    = toScreens $ Disabled a b c
disconnected a b  = toScreens $ Disconnected a b

allSecondaryOff = onAllScreens disableSecondary

allScreensLeft = onAllScreens $ modifyPosition $ const LeftOf

allScreensRight = onAllScreens $ modifyPosition $ const RightOf

nextScreenRotation n = onAllScreens $ modifyRotationOfScreen n succ

prevScreenRotation n = onAllScreens $ modifyRotationOfScreen n pred

autoEnable :: ScreenF a -> ScreenF a
autoEnable (Disabled n c s) = Secondary n c LeftOf s
autoEnable x = x

disableSecondary :: ScreenF a -> ScreenF a
disableSecondary (Secondary n c _ s) = Disabled n c s
disableSecondary x = x

modifyRotationOfScreen :: OutputName -> (Rotation -> Rotation) -> ScreenF a -> ScreenF a
modifyRotationOfScreen n = modifyScreenAt n . modifyRotation

modifyRotation :: (Rotation -> Rotation) -> ScreenF a -> ScreenF a
modifyRotation f = modifyConfig $ \c -> c { rotation = f $ rotation c }

modifyConfig :: (Config -> Config) -> ScreenF a -> ScreenF a
modifyConfig f = onSecondary $ \(c, p) -> (f c, p)

modifyPosition :: (Position -> Position) -> ScreenF a -> ScreenF a
modifyPosition f = onSecondary $ \(c, p) -> (c, f p)

onSecondary :: ((Config, Position) -> (Config, Position)) -> ScreenF a -> ScreenF a
onSecondary f (Secondary n c p s) = let (c', p') = f (c,p) in Secondary n c' p' s
onSecondary _ x = x

modifyScreenAt :: OutputName -> (ScreenF a -> ScreenF a) -> ScreenF a -> ScreenF a
modifyScreenAt name f x
  | getOutputName x == name = f x
  | otherwise               = x

getOutputName :: ScreenF a -> OutputName
getOutputName (Primary n _)       = n
getOutputName (Secondary n _ _ _) = n
getOutputName (Disabled n _ _)    = n
getOutputName (Disconnected n _)  = n

nextOutputName :: ScreenF OutputName -> OutputName
nextOutputName (Primary n _)       = n
nextOutputName (Secondary n _ _ _) = n
nextOutputName (Disabled _ _ n)    = n
nextOutputName (Disconnected _ n)  = n
