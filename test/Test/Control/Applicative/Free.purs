module Test.Control.Applicative.Free
  ( checkAnalyze
  , checkStack
  , check
  ) where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, analyzeFreeAp, retractFreeAp)
import Control.Applicative.Free.Gen as GenF
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Type.Proxy (Proxy(..))


data M r = A r | B r

ma :: FreeAp M Unit
ma = liftFreeAp (A unit)

mb :: FreeAp M Unit
mb = liftFreeAp (B unit)

printM :: forall a. FreeAp M a -> String
printM fr =
  analyzeFreeAp go fr
  where
  go (A _) = "A"
  go (B _) = "B"

build :: Int -> FreeAp M Unit -> FreeAp M Unit -> FreeAp M Unit
build 0 _ acc = acc
build n x acc = build (n - 1) x (acc *> x)

buildExpected :: Int -> String -> String -> String
buildExpected 0 _ acc = acc
buildExpected n x acc = buildExpected (n - 1) x (acc <> x)

checkAnalyze :: Either String String
checkAnalyze =
  if result == expected
    then Right result
    else Left (result <> " is not " <> expected)
  where
  result :: String
  result = printM (build 10 (ma *> mb) mb)

  expected :: String
  expected = buildExpected 10 "AB" "B"


checkStack :: Either String String
checkStack =
  if result == expected
    then Right "safe for 100000 node"
    else Left (result <> " is not " <> expected)
  where
  result :: String
  result = printM (build 100000 (ma *> mb) mb)

  expected :: String
  expected = buildExpected 100000 "AB" "B"


newtype ArbFreeAp a = ArbFreeAp (FreeAp (Tuple (Array String)) a)

instance arbitraryArbFreeAp :: (Coarbitrary a, Arbitrary a) => Arbitrary (ArbFreeAp a) where
  arbitrary = ArbFreeAp <$>
    GenF.genFree
      arbitrary
      (arbitrary :: Gen a)
      (arbitrary :: Gen (a -> a))

instance eqArbFreeAp :: Eq a => Eq (ArbFreeAp a) where
  eq (ArbFreeAp a) (ArbFreeAp b) = retractFreeAp a == retractFreeAp b

derive newtype instance functorArbFreeAp :: Functor ArbFreeAp
derive newtype instance applyArbFreeAp :: Apply ArbFreeAp
derive newtype instance applicativeArbFreeAp :: Applicative ArbFreeAp

check ∷ Effect Unit
check = checkLaws "FreeAp" do
  Data.checkEq prxFree
  Data.checkFunctor prx2Free
  Control.checkApply prx2Free
  Control.checkApplicative prx2Free
  where
  prxFree = Proxy ∷ Proxy (ArbFreeAp A)
  prx2Free = Proxy ∷ Proxy ArbFreeAp
