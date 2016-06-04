module Test.Main where

import Prelude (Unit, bind)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Test.Control.Applicative.Free.Validation as Validation
import Test.Control.Applicative.Free as FreeTest

main :: Eff (console :: CONSOLE) Unit
main = do
  log "\nvalid case:"
  logShow (Validation.runForm "Joe" "Smith" "28")

  log "\nempty last name:"
  logShow (Validation.runForm "Larry" "" "45")

  log "\ninvalid age:"
  logShow (Validation.runForm "Sue" "Larry" "A")

  log "\nanalyze:"
  logShow FreeTest.checkAnalyze
