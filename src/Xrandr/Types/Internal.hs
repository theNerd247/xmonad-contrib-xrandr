{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Xrandr.Types.Internal 
  ( Screens
  , ScreenF (..)
  , Position (..)
  , Rotation (..)
  , Config (..)
  , OutputName (..)
  , Modes
  , Mode (..)
  )
where

import Data.String (IsString)
import Data.Zipper
import Data.Functor.Foldable
import qualified Data.Text as T
import Numeric.Natural

data ScreenF b = 
    Primary      OutputName Config
  | Secondary    OutputName Config Position b
  | Disabled     OutputName Config b
  | Disconnected OutputName b
  deriving (Show, Functor)

type Screens = Fix ScreenF

data Position =
    RightOf
  | LeftOf 
  | Above  
  | Below  
  | SameAs 
  deriving (Show, Eq, Ord, Enum)

instance (ToCmd a) => ToCmd (Zipper a) where
  buildCmd = buildCmd . focus

newtype OutputName = OutputName { name :: T.Text } 
  deriving (Show, Eq, Ord, IsString)

instance ToCmd OutputName where
  buildCmd n = ["--output", name n]

data Config = Config
  { rotation  :: Rotation
  , modes     :: Modes
  } deriving (Show, Eq, Ord)

instance ToCmd Config where
  buildCmd m = 
       (buildCmd . modes $ m)
    <> (buildCmd . rotation $ m)
    
type Modes = Zipper Mode

newtype Mode = Mode { modeName :: (Natural, Natural) }
  deriving (Show, Eq, Ord)

instance ToCmd Mode where
  buildCmd m =
    [ "--mode"
    , (T.pack . show . modeX $ m) <> "x" <> (T.pack . show . modeY $ m)
    ]

data Rotation =
    Normal
  | RotateLeft
  | RotateRight
  | Inverted
  deriving (Show, Eq, Ord, Enum)

instance ToCmd Rotation where
  buildCmd x = ["--rotate", rotateOption x]
    where
      rotateOption Normal      = "normal"
      rotateOption RotateLeft  = "left"
      rotateOption RotateRight = "right"
      rotateOption Inverted    = "inverted"

modeX = fst . modeName

modeY = snd . modeName

primary a b       = Fix $ Primary a b
secondary a b c d = Fix $ Secondary a b c d
disabled a b c    = Fix $ Disabled a b c
disconnected a b  = Fix $ Disconnected a b

autoEnable' :: ScreenF Screens -> Screens
autoEnable' (Disabled n c s) = secondary n c LeftOf s
autoEnable' x = Fix x

allScreensOff' :: ScreenF Screens -> Screens
allScreensOff' (Secondary n c _ s) = disabled n c s
allScreensOff' x = Fix x

setSecondaryPositions :: Position -> ScreenF a -> ScreenF a
setSecondaryPositions = modifyPositions . const

modifyPositions :: (Position -> Position) -> ScreenF a -> ScreenF a
modifyPositions f = onSecondary $ \(n, c, p) -> (n, c, f p)

modifyRotation :: (Rotation -> Rotation) -> ScreenF a -> ScreenF a
modifyRotation f = modifyConfigs $ \c -> c { rotation = f $ rotation c }

modifyConfigs :: (Config -> Config) -> ScreenF a -> ScreenF a
modifyConfigs f = onSecondary $ \(n, c, p) -> (n, f c, p)

onSecondary :: (OutputName, Config, Position) -> (OutputName, Config, Position) -> ScreenF a -> ScreenF a
onSecondary f (Secondary n c p s) = let (n', c', p') = f (n,c,p) in Secondary n' c' p' s
onSecondary _ x = x

modifyScreenAt :: (ScreenF a -> ScreenF a) -> OutputName -> ScreensF a -> ScreenF a
modifyScreenAt f name x
  | getOutputName x == name = f x
  | otherwise               = x

configWithNormalRotation :: Modes -> Config
configWithNormalRotation = Config Normal

getOutputName :: ScreenF a -> OutputName
getOutputName (Primary n _)       = n
getOutputName (Secondary n _ _ _) = n
getOutputName (Disabled n _ _)    = n
getOutputName (Disconnected n _)  = n

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

nextOutputName' :: ScreenF OutputName -> OutputName
nextOutputName' (Primary n _)       = n
nextOutputName' (Secondary n _ _ _) = n
nextOutputName' (Disabled _ _ n)    = n
nextOutputName' (Disconnected _ n)  = n
