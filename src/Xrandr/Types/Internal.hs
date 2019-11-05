{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Xrandr.Types.Internal 
  ( Screens
  , ScreenF (..)
  , ScreenCmd
  , Position (..)
  , Rotation (..)
  , Config (..)
  , OutputName (..)
  , Modes
  , Mode (..)
  , modeX
  , modeY
  , fromScreens
  , toScreens
  )
where

import Data.Data
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
  deriving (Show, Functor, Typeable)

type Screens = Fix ScreenF

data Position =
    RightOf
  | LeftOf 
  | Above  
  | Below  
  | SameAs 
  deriving (Show, Eq, Ord, Enum, Bounded)

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
  deriving (Show, Eq, Ord, Enum, Bounded)

modeX = fst . modeName

modeY = snd . modeName

type ScreenCmd a = ScreenF a -> ScreenF a

toScreens :: ScreenF Screens -> Screens
toScreens = Fix

fromScreens :: (ScreenF a -> a) -> Screens -> a
fromScreens = cata
