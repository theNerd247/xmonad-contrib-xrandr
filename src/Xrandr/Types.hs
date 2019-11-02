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
  , modeX
  , modeY
  -- | default config
  , configWithNormalRotation
  -- | screens combinators
  , autoEnable
  , setSecondaryPositions
  , allScreensOff
  , allScreensLeft
  , allScreensRight
  , makeCmd
  , buildCmd
  )
where

import Xrandr.Types.Internal
import Data.Functor.Foldable
import Xrandr.Cmd.Class
import Data.Zipper
import qualified Data.Text as T

instance ToCmd OutputName where
  buildCmd n = ["--output", name n]

instance ToCmd Config where
  buildCmd m = 
       (buildCmd . modes $ m)
    <> (buildCmd . rotation $ m)
    
instance ToCmd Mode where
  buildCmd m =
    [ "--mode"
    , (T.pack . show . modeX $ m) <> "x" <> (T.pack . show . modeY $ m)
    ]

instance ToCmd Rotation where
  buildCmd x = ["--rotate", rotateOption x]
    where
      rotateOption Normal      = "normal"
      rotateOption RotateLeft  = "left"
      rotateOption RotateRight = "right"
      rotateOption Inverted    = "inverted"

instance (ToCmd a) => ToCmd (Zipper a) where
  buildCmd = buildCmd . focus

modeX = fst . modeName

modeY = snd . modeName

primary a b       = Fix $ Primary a b
secondary a b c d = Fix $ Secondary a b c d
disabled a b c    = Fix $ Disabled a b c
disconnected a b  = Fix $ Disconnected a b

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

configWithNormalRotation :: Modes -> Config
configWithNormalRotation = Config Normal

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