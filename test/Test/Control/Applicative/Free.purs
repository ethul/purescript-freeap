module Test.Control.Applicative.Free
  ( checkAnalyze
  ) where

import Prelude (Unit, (==), (<>), unit, apply, map)
import Control.Applicative.Free (FreeAp, liftFreeAp, analyzeFreeAp)
import Data.Either (Either(..))

data M r = A r | B r

ma :: FreeAp M Unit
ma = liftFreeAp (A unit)

mb :: FreeAp M Unit
mb = liftFreeAp (B unit)

printM :: forall a. FreeAp M a -> String
printM = analyzeFreeAp go where
  go (A _) = "A"
  go (B _) = "B"

result :: String
result = printM (apply (map (\l r -> r) ma) mb)

expected :: String
expected = "AB"

checkAnalyze :: Either String String
checkAnalyze = if result == expected
               then Right result
               else Left (result <> " is not " <> expected)
