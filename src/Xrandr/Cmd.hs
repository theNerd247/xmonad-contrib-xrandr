module Xrandr.Cmd
  ( makeCmd
  )
where

import Xrandr.Cmd.Class
import Xrandr.Types
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

makeCmd = fromScreens makeCommands'

makeCommands' :: ScreenF (Screens, Cmd) -> Cmd
makeCommands' (Primary n c)                     =         (buildCmd n) <> (buildCmd c) <> ["--primary"]
makeCommands' (Secondary n c p (screens, cmds)) = cmds <> (buildCmd n) <> (buildCmd c) <> [positionArg p, name $ nextOutputName screens]
makeCommands' (Disabled n _ (_, cmds))          = cmds <> (buildCmd n)                 <> ["--off"]
makeCommands' (Disconnected n (_, cmds))        = cmds <> (buildCmd n)                 <> ["--off"]

positionArg :: Position -> T.Text
positionArg LeftOf  = "--left-of"
positionArg RightOf = "--right-of"
positionArg Above   = "--above"
positionArg Below   = "--below"
positionArg SameAs  = "--same-as"
