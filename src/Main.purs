module Main where

import Prelude

import Data.List as List
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Options.Applicative (Parser, execParser, helper, idm, info, subparser)
import PureShell.Cat.Program as Cat

foreign import argv :: Array String

args :: List.List String
args = List.drop 2 $ List.fromFoldable argv

commandsWithHelper :: Parser (Aff.Aff Unit)
commandsWithHelper = helper <*> commands where
  commands = subparser Cat.program

main :: Effect Unit
main = Aff.launchAff_ do
  join $ liftEffect $ execParser (info commandsWithHelper idm)