module PureShell.Common.Utility where

import Prelude

import Data.Array (head)
import Data.Maybe (fromMaybe)
import Data.String.Regex (Regex, replace')
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Console (log)

unbacklash :: String -> String
unbacklash = replace' charsToEscape replaceImpl
  where
    charsToEscape :: Regex
    charsToEscape = unsafeRegex """\\([\\rnt'"])""" global
    replaceImpl :: String -> Array String -> String
    replaceImpl _ xs
      | xs == ["n"] = "\n"
      | xs == ["r"] = "\r"
      | xs == ["t"] = "\t"
      | xs == ["\\"] = "\\"
      | otherwise = fromMaybe "" (head xs)

logEscape :: String -> Effect Unit
logEscape = unbacklash >>> log

logShowEscape :: ∀ a. Show a => a -> Effect Unit
logShowEscape = show >>> logEscape