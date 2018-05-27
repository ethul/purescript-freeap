module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Test.Control.Applicative.Free as FreeTest
import Test.Control.Applicative.Free.Validation as Validation

main :: Effect Unit
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
