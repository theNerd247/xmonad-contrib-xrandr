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
  , allScreensOff
  , allScreensLeft
  , allScreensRight
  , nextScreenRotation
  , prevScreenRotation
  )
where

import qualified Xrandr.Types.Internal as I

allScreensOff = runScreens I.allScreensOff

allScreensLeft = runScreens I.allScreensLeft

allScreensRight = runScreens I.allScreensRight

nextScreenRotation = runScreens $ I.modifyRotationOfScreen next

prevScreenRotation = runScreens $ I.modifyRotationOfScreen prev
