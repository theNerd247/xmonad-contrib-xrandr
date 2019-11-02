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
  deriving (Show)

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
  deriving (Show, Eq, Ord)
