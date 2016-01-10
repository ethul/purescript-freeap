module Test.Main where

import Prelude (Unit(), bind)

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log, print)

import qualified Test.Control.Applicative.Free.Validation as Validation
import qualified Test.Control.Applicative.Free as FreeTest

main :: Eff (console :: CONSOLE) Unit
main = do
  log "\nvalid case:"
  print (Validation.runForm "Joe" "Smith" "28")

  log "\nempty last name:"
  print (Validation.runForm "Larry" "" "45")

  log "\ninvalid age:"
  print (Validation.runForm "Sue" "Larry" "A")

  log "\nanalyze:"
  print FreeTest.checkAnalyze
