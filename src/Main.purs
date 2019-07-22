module Main where

import Prelude

import Control.Alt ((<|>))
import Data.List as List
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Options.Applicative (Parser, execParser, helper, hsubparser, idm, info)
import PureShell.AppM (AppM, runAppM)
import PureShell.Cat.Program as Cat
import PureShell.Ls.Program as Ls

foreign import argv :: Array String

args :: List.List String
args = List.drop 2 $ List.fromFoldable argv

commandsWithHelper :: Parser (AppM Unit)
commandsWithHelper = helper <*> commands where
  commands =
        hsubparser Cat.program
    <|> hsubparser Ls.program

main :: Effect Unit
main = Aff.launchAff_ do
  execParser (info commandsWithHelper idm)
    # liftEffect
    # join
    # runAppM