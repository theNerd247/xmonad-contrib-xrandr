{-# LANGUAGE DeriveFunctor #-}

module Xrandr.Types.Internal 
  ( Screens
  , Position
  , Rotation
  , Config (..)
  , OutputName (..)
  , Modes
  )
where

import Data.Zipper
import Data.Functor.Foldable
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
  deriving (Show)

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
  deriving (Show, Eq, Ord)

instance ToCmd Rotation where
  buildCmd x = ["--rotate", rotateOption x]
    where
      rotateOption Normal      = "normal"
      rotateOption RotateLeft  = "left"
      rotateOption RotateRight = "right"
      rotateOption Inverted    = "inverted"
