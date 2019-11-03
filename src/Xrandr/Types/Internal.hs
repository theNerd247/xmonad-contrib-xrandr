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
  , modeX
  , modeY
  , toScreens
  , fromScreens
  , withScreens 
  , onAllScreens
  )
where

import Data.Functor.Foldable
import Data.String (IsString)
import Data.Zipper
import Numeric.Natural
import qualified Data.Text as T

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

newtype OutputName = OutputName { name :: T.Text } 
  deriving (Show, Eq, Ord, IsString)

data Config = Config
  { rotation  :: Rotation
  , modes     :: Modes
  } deriving (Show, Eq, Ord)

type Modes = Zipper Mode

newtype Mode = Mode { modeName :: (Natural, Natural) }
  deriving (Show, Eq, Ord)

data Rotation =
    Normal
  | RotateLeft
  | RotateRight
  | Inverted
  deriving (Show, Eq, Ord, Enum)

modeX = fst . modeName

modeY = snd . modeName

toScreens :: ScreenF Screens -> Screens
toScreens = Fix

withScreens :: (ScreenF (Screens, a) -> a) -> Screens -> a
withScreens = para

fromScreens :: (ScreenF a -> a) -> Screens -> a
fromScreens = cata

onAllScreens :: (ScreenF Screens -> ScreenF Screens) -> Screens -> Screens
onAllScreens = fromScreens . (toScreens .)
