module Xrandr.Cmd 
  ( Cmd
  )
where

import qualified Data.Text as T

type Cmd = [T.Text]

class ToCmd a where
  buildCmd :: a -> Cmd

instance (ToCmd a) => ToCmd (Maybe a) where
  buildCmd = maybe mempty buildCmd
