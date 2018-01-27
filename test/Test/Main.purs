module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Test.Control.Applicative.Free as FreeTest
import Test.Control.Applicative.Free.Validation as Validation

main :: Eff (console ∷ CONSOLE , random ∷ RANDOM , exception ∷ EXCEPTION) Unit
main = do
  log "\nvalid case:"
  logShow (Validation.runForm "Joe" "Smith" "28")

  log "\nempty last name:"
  logShow (Validation.runForm "Larry" "" "45")

  log "\ninvalid age:"
  logShow (Validation.runForm "Sue" "Larry" "A")

  log "\nanalyze:"
  logShow FreeTest.checkAnalyze
  
  log "\nstack safety:"
  logShow FreeTest.checkStack
  
  log "\nlaws:"
  FreeTest.check
