{-# LANGUAGE OverloadedStrings #-}

module Xrandr.Cmd
  ( makeCmd
  , Cmd
  )
where

import Xrandr.Cmd.Class
import Xrandr.Types
import Xrandr.Types.Internal
import Data.Zipper
import qualified Data.Text as T

instance (ToCmd a) => ToCmd (Zipper a) where
  buildCmd = buildCmd . focus

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

makeCmd :: ScreenF (OutputName, Cmd) -> (OutputName, Cmd)
makeCmd (Primary n c)                = (n,         (buildCmd n) <> (buildCmd c) <> ["--primary"])
makeCmd (Secondary n c p (ln, cmds)) = (n, cmds <> (buildCmd n) <> (buildCmd c) <> [positionArg p, name ln])
makeCmd (Disabled n _ (_, cmds))     = (n, cmds <> (buildCmd n)                 <> ["--off"])
makeCmd (Disconnected n (_, cmds))   = (n, cmds <> (buildCmd n)                 <> ["--off"])

positionArg :: Position -> T.Text
positionArg LeftOf  = "--left-of"
positionArg RightOf = "--right-of"
positionArg Above   = "--above"
positionArg Below   = "--below"
positionArg SameAs  = "--same-as"
