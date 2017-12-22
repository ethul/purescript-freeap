module Test.Control.Applicative.Free
  ( checkAnalyze
  ) where

-- import Prelude (Unit, (==), (<>), unit, (-), ($), (>>>))
import Prelude

import Control.Applicative ((*>))
import Control.Applicative.Free (FreeAp, liftFreeAp, analyzeFreeAp, retractFreeAp)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Show (class Show)
import Debug.Trace (spy)

data M r = A r | B r

instance showFreeAp :: Show (M a) where
  show (A _) = "A"
  show (B _) = "B"

fInc = liftFreeAp (Identity (_ + 1))
fOne = liftFreeAp (Identity 1)
fMult = liftFreeAp (Identity (+))

x = fMult <*> (fInc <*> fOne) <*> (fInc <*> fOne)

zzz = spy { res1: retractFreeAp x }

ma :: FreeAp M Unit
ma = liftFreeAp (A unit)

mb :: FreeAp M Unit
mb = liftFreeAp (B unit)

printM :: forall a. FreeAp M a -> String
printM fr =
  analyzeFreeAp (go >>> spy) fr
  where
  go (A _) = "A"
  go (B _) = "B"

build :: Int -> FreeAp M Unit -> FreeAp M Unit -> FreeAp M Unit
build 0 _ acc = acc
build n x acc = build (n - 1) x (acc *> x)

result :: String
result = printM (build 10 (ma *> mb) mb)
-- result = printM ((mb *> mb) *> ma)
-- result = printM (mb *> (mb *> ma))

buildExpected :: Int -> String -> String -> String
buildExpected 0 _ acc = acc
buildExpected n x acc = buildExpected (n - 1) x (acc <> x)

expected :: String
expected = buildExpected 10 "AB" "B"

checkAnalyze :: Either String String
checkAnalyze = if result == expected
               then Right result
               else Left (result <> " is not " <> expected)
