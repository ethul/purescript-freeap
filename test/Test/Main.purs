module Test.Main where

import Prelude (Unit(), bind)

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), print)

import qualified Test.Control.Applicative.Free.Validation as Validation

main :: Eff (console :: CONSOLE) Unit
main = do
  print (Validation.runForm "Joe" "Smith" "28")

  print (Validation.runForm "Larry" "" "45")

  print (Validation.runForm "Sue" "Larry" "A")
