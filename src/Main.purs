module Main where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import PureShell.Cat.Cat (handleCatArgs)

foreign import argv :: Array String

args :: List String
args = List.drop 2 $ List.fromFoldable argv

main :: Effect Unit
main = Aff.launchAff_ do
  case args of
    "cat" : rest -> handleCatArgs rest
    _ -> do
      liftEffect $ log "🍝"
