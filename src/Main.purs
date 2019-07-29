module Main where

import Prelude

import Control.Alt ((<|>))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Options.Applicative (Parser, execParser, helper, hsubparser, idm, info)
import PureShell.AppM (AppM, runAppM)
import PureShell.Cat.Program as Cat
import PureShell.Ls.Program as Ls
import PureShell.Mv.Program as Mv

commandsWithHelper :: Parser (AppM Unit)
commandsWithHelper = helper <*> commands where
  commands =
        hsubparser Cat.program
    <|> hsubparser Ls.program
    <|> hsubparser Mv.program

main :: Effect Unit
main = Aff.launchAff_ do
  execParser (info commandsWithHelper idm)
    # liftEffect
    # join
    # runAppM