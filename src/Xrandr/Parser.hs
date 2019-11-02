{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Xrandr.Parser 
  ( parseScreens
  )
where

import Data.Attoparsec.Text
import Xrandr.Types
import Data.PreList
import Numeric.Natural (Natural)
import Control.Applicative
import Data.Zipper
import qualified Data.Text as T

parseScreens = parseOnly $ screenTexts
-- DO NOT add endOfInput here. We don't want the parser to match to entire output
-- from xrandr; only just enough text to determine the shape of the screens

screenTexts :: Parser Screens
screenTexts = ignoreRestOfLine *> manyPrelist screenText

manyPrelist :: Parser (PreList a) -> Parser a
manyPrelist = applyPrelist . many

screenText :: Parser (PreList Screens)
screenText = 
      (EndoVal <$> isPrimary)
  <|> (EndoF   <$> isSecondary)
  <|> (EndoF   <$> isDisabled)
  <|> (EndoF   <$> isDisconnected)
  
isDisconnected :: Parser (Screens -> Screens)
isDisconnected = disconnected <$> disconnectedOutputName

isPrimary :: Parser (Screens)
isPrimary = 
  primary
  <$> connectedOutputNameAndPrimary
  <*> (configTextWith enabledModesText)

isSecondary :: Parser (Screens -> Screens)
isSecondary =
  secondary
  <$> connectedOutputNameAndNotPrimary
  <*> (configTextWith enabledModesText)
  <*> (pure LeftOf)

isDisabled :: Parser (Screens -> Screens)
isDisabled = 
  disabled
  <$> connectedOutputNameAndNotPrimary
  <*> (configTextWith disabledModesText)

disconnectedOutputName :: Parser OutputName 
disconnectedOutputName = outputNameWith "disconnected" isNotPrimaryText

connectedOutputNameAndPrimary :: Parser OutputName
connectedOutputNameAndPrimary = connectedOutputName isPrimaryText

connectedOutputNameAndNotPrimary :: Parser OutputName
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


