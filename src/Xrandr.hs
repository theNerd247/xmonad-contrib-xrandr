{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Xrandr where

import Control.Applicative
import Control.Arrow
import Control.Monad ((>=>))
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
import Data.Functor.Foldable
import Data.List (partition, find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Control.Monad
import Data.String
import Numeric.Natural
import Prelude hiding (takeWhile, take)
import Shelly (shelly, run, run_)
import qualified Data.Text as T

-- | Requirements
--
--  * Screens can be either: Connected or Disconnected
--  * Connected Screens can be Enabled or Disabled
--  * Enabled screens must have a Mode and Rotation defined
--  * Default rotation is Normal which means the screen is upright).
--  * Only 1 screen can be the Primary, and all the others must be Secondary
--  * Disabled / Disconnected screens must not have a position
--  * Connected Screens must have a position relative to another screen
--      Secondary -> Primary | Secondary; Primary doesn't have a position
--
--  * Disabled and Disconnected screens should coorespond to the command
--    `--output <OutputName> --off`
--  * We would like to keep the Disabled information for later. When the screen
--    state expression is used in a State monad this will allow for disabling
--    secondary screens that are currently enabled.
--  * 

-- | represents the physical layout of those screens which are connected
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

data Zipper a = Zipper
  { focus :: a
  , next :: [a]
  , prev :: [a]
  } deriving (Show, Eq, Ord, Functor)

left :: Zipper a -> Zipper a
left x@(Zipper _ _ []) = x
left (Zipper n ns (p:ps)) = Zipper p (n:ns) ps

right :: Zipper a -> Zipper a
right x@(Zipper _ [] _) = x
right (Zipper p (n:ns) ps) = Zipper n ns (p:ps)

moveUntil :: (a -> a) -> (a -> Bool) -> a -> a
moveUntil move p = hylo (either id id) (moveIf <*> p) 
  where
    moveIf a True  = Left a
    moveIf a False = Right . move $ a

type Modes = Zipper Mode

newtype Mode = Mode { modeName :: (Natural, Natural) }
  deriving (Show, Eq, Ord)

data Rotation =
    Normal
  | RotateLeft
  | RotateRight
  | Inverted
  deriving (Show, Eq, Ord)

type Cmd = [T.Text]

class ToCmd a where
  buildCmd :: a -> Cmd

instance ToCmd OutputName where
  buildCmd n = ["--output", name n]

instance ToCmd Config where
  buildCmd m = 
       (buildCmd . modes $ m)
    <> (buildCmd . rotation $ m)
    
instance ToCmd Rotation where
  buildCmd x = ["--rotate", rotateOption x]
    where
      rotateOption Normal      = "normal"
      rotateOption RotateLeft  = "left"
      rotateOption RotateRight = "right"
      rotateOption Inverted    = "inverted"

instance (ToCmd a) => ToCmd (Maybe a) where
  buildCmd = maybe mempty buildCmd

instance (ToCmd a) => ToCmd (Zipper a) where
  buildCmd = buildCmd . focus

instance ToCmd Mode where
  buildCmd m =
    [ "--mode"
    , (T.pack . show . modeX $ m) <> "x" <> (T.pack . show . modeY $ m)
    ]

modeX = fst . modeName

modeY = snd . modeName

primary a b = Fix $ Primary a b
secondary a b c d = Fix $ Secondary a b c d
disabled a b c = Fix $ Disabled a b c
disconnected a b = Fix $ Disconnected a b

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

allScreensLeft :: Screens -> Screens
allScreensLeft = setSecondaryPositions LeftOf

allScreensRight :: Screens -> Screens
allScreensRight = setSecondaryPositions RightOf

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

configWithNormalRotation :: Modes -> Config
configWithNormalRotation = Config Normal

data PreList a = 
    EndoVal a
  | EndoF (a -> a)

instance Semigroup (PreList a) where
   (EndoVal s) <> (EndoVal _) = EndoVal s
   (EndoF f)  <> (EndoF g)  = EndoF $ g . f
   (EndoVal s) <> (EndoF f)  = EndoVal $ f s
   (EndoF f)  <> (EndoVal s) = EndoVal $ f s

instance Monoid (PreList a) where
  mempty = EndoF id

unPrelist (EndoVal a) = Just a
unPrelist (EndoF _) = Nothing

manyPrelist :: Parser (PreList a) -> Parser a
manyPrelist = maybe empty pure <=< (fmap (unPrelist . mconcat) . many)

fixedEndoVal = EndoVal . Fix
fixedEndoF f = EndoF $ Fix . f

type SC = PreList Screens

-- | NOTE: you may need to include a *> ignoreRestOfLine or endOfInput in here....
--parseScreens :: T.Text -> Either String Screens
parseScreens = parseOnly $ screenTexts
-- DO NOT add endOfInput here. We don't want the parser to match to entire output
-- from xrandr; only just enough text to determine the shape of the screens

screenTexts :: Parser Screens
screenTexts = ignoreRestOfLine *> manyPrelist screenText

screenText :: Parser SC
screenText = 
      (fixedEndoVal <$> isPrimary)
  <|> (fixedEndoF <$> isSecondary)
  <|> (fixedEndoF <$> isDisabled)
  <|> (fixedEndoF <$> isDisconnected)
  
isDisconnected :: Parser (b -> ScreenF b)
isDisconnected = Disconnected <$> disconnectedOutputName

isPrimary :: Parser (ScreenF b)
isPrimary = 
  Primary
  <$> connectedOutputNameAndPrimary
  <*> (configTextWith enabledModesText)

isSecondary :: Parser (b -> ScreenF b)
isSecondary =
  Secondary
  <$> connectedOutputNameAndNotPrimary
  <*> (configTextWith enabledModesText)
  <*> (pure LeftOf)

isDisabled :: Parser (b -> ScreenF b)
isDisabled = 
  Disabled
  <$> connectedOutputNameAndNotPrimary
  <*> (configTextWith disabledModesText)

disconnectedOutputName = outputNameWith "disconnected" isNotPrimaryText

connectedOutputNameAndPrimary = connectedOutputName isPrimaryText

connectedOutputNameAndNotPrimary = connectedOutputName isNotPrimaryText

connectedOutputName :: Parser a -> Parser OutputName
connectedOutputName = outputNameWith "connected"

isPrimaryText = skipSpace *> string "primary" <* skipSpace

isNotPrimaryText = char ' ' *> pure () <|> endOfLine

outputNameWith :: T.Text -> Parser a -> Parser OutputName
outputNameWith connection primary =
  outputNameText
  <* skipSpace
  <* string connection
  <* primary
  <* ignoreRestOfLine

outputNameText :: Parser OutputName
outputNameText = OutputName <$> takeTill (==' ')

configTextWith :: Parser Modes -> Parser Config
configTextWith = fmap configWithNormalRotation

enabledModesText = modesTextWith enabledModeLine

disabledModesText = modesTextWith preferredModeLine

modesTextWith :: Parser Mode -> Parser Modes
modesTextWith focusedMode = 
  (flip Zipper)
  <$> (many disabledModeLine)
  <*> focusedMode
  <*> (many disabledModeLine)

disabledModeLine :: Parser Mode
disabledModeLine = modeLineWith $ isDisabledMode

enabledModeLine :: Parser Mode
enabledModeLine = modeLineWith isEnabledMode

preferredModeLine :: Parser Mode
preferredModeLine = modeLineWith isPreferredMode

modeLineWith :: Parser () -> Parser Mode
modeLineWith enabled = 
  skipSpace
  *> modeText 
  <*  skipSpace
  <*  (natText *> char '.' *> natText)
  <* enabled
  <* ignoreRestOfLine

modeText :: Parser Mode
modeText = 
  ((Mode .) <$> (,))
  <$> natText 
  <* char 'x' 
  <*> natText
  <* option () (char 'i' *> pure ())

isEnabledMode :: Parser ()
isEnabledMode = char '*' *> pure ()

isDisabledMode :: Parser ()
isDisabledMode = string "  " *> pure ()

isPreferredMode :: Parser ()
isPreferredMode = string " +" *> pure ()

natText :: Parser Natural
natText = read <$> many1 digit <?> "natText"

ignoreRestOfLine :: Parser ()
ignoreRestOfLine = takeTill isEndOfLine *> endOfLine

optionalFlag :: (Monoid a) => a -> Parser b -> Parser a
optionalFlag a p = flag a p <|> pure mempty

flag :: a -> Parser b -> Parser a
flag a p = p *> pure a

spawnXrandr :: (Screens -> Screens) -> IO ()
spawnXrandr f = 
  readCurrentScreens >>= either print (runXrandrCmdWith f) 

readCurrentScreens :: IO (Either String Screens)
readCurrentScreens = parseScreens <$> (runXrandr [])

runXrandrCmdWith :: (Screens -> Screens) -> Screens -> IO ()
runXrandrCmdWith f = runXrandr . makeCmd . f >=> const (return ())
 
runXrandr :: [T.Text] -> IO T.Text
runXrandr = shelly . run "xrandr"
