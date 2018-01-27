module Control.Applicative.Free.Gen where

import Prelude

import Control.Applicative.Free as F
import Control.Monad.Gen (class MonadGen, oneOf)
import Control.Monad.Rec.Class (class MonadRec)
import Data.NonEmpty (NonEmpty(..))

genFree :: forall m f a
   . MonadGen m
  => MonadRec m
  => m (f Unit)
  -> m a
  -> m (a -> a)
  -> m (F.FreeAp f a)
genFree genF genA genA2A = oneOf $ NonEmpty 
    ( genA <#> pure)
    [ do 
        fUnit <- genF
        a <- genA
        pure $ 
          pure (const a) <*> F.liftFreeAp fUnit
    , do 
        fUnit <- genF
        a <- genA
        a2a <- genA2A
        pure $ 
          (pure (const a) <*> F.liftFreeAp fUnit) <#> a2a
    , do
        fUnit <- genF
        a <- genA
        a2a <- genA2A
        pure $
          F.liftFreeAp fUnit <#> const a <#> a2a
    , do
        a <- genA
        a2a <- genA2A
        pure $ pure a <#> a2a
    ]
