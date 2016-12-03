module Test.Main where

import Prelude (Unit, bind)

import Data.Either (Either(..))

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Control.Applicative.Free.Validation as Validation
import Test.Control.Applicative.Free as FreeTest

import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff.  Eff (avar :: AVAR, console :: CONSOLE, testOutput :: TESTOUTPUT | eff) Unit
main = runTest do
  suite "validation" do
    test "valid input" do
      Assert.equal (Right (Validation.User { firstName: "Joe", lastName: "Smith", age: 28 }))
                   (Validation.runForm "Joe" "Smith" "28")

    test "empty last name" do
      Assert.equal (Left "Last name: Invalid NES")
                   (Validation.runForm "Larry" "" "45")

    test "invalid age" do
      Assert.equal (Left "Age: Invalid Int")
                   (Validation.runForm "Sue" "Larry" "A")

{-
  suite "analyze" do
    test "checkAnalyze" do
      Assert.equal (Right "AB")
                   FreeTest.checkAnalyze
-}
