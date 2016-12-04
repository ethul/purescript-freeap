-- | This module defines a free applicative functor.
-- |
-- | The implementation of this module is based on Exequiel Rivas and Mauro
-- | Jaskelioff's work.
-- |
-- | See [Notions of Computation as Monoids](http://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids.pdf) (Rivas and Jaskelioff 2016)
module Control.Applicative.Free
  ( FreeAp
  , liftFreeAp
  , retractFreeAp
  , foldFreeAp
  , hoistFreeAp
  , analyzeFreeAp
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Functor.Day (type (⊗), day, runDay)
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | The free applicative functor for a type constructor `f`.
data FreeAp f a = Pure a | Ap ((f ⊗ FreeAp f) a)

-- | Lift a value described by the type constructor `f` into
-- | the free applicative functor.
liftFreeAp :: forall f a. f a -> FreeAp f a
liftFreeAp a = Ap (day const a (Pure unit))

-- | Run a free applicative functor using the applicative instance for
-- | the type constructor `f`.
retractFreeAp :: forall f a. Applicative f => FreeAp f a -> f a
retractFreeAp (Pure a) = pure a
retractFreeAp (Ap d) = runDay (\i f g -> i <$> f <*> retractFreeAp g) d

-- | Run a free applicative functor with a natural transformation from
-- | the type constructor `f` to the applicative functor `g`.
foldFreeAp :: forall f g a. Applicative g => (f ~> g) -> FreeAp f a -> g a
foldFreeAp _ (Pure a) = pure a
foldFreeAp k (Ap d) = runDay (\i f g -> pure i <*> k f <*> foldFreeAp k g) d


-- | Natural transformation from `FreeAp f a` to `FreeAp g a` given a
-- | natural transformation from `f` to `g`.
hoistFreeAp :: forall f g a. (f ~> g) -> FreeAp f a -> FreeAp g a
hoistFreeAp _ (Pure a) = Pure a
hoistFreeAp k (Ap d) = runDay (\i f g -> Ap (day i (k f) (hoistFreeAp k g))) d

-- | Perform monoidal analysis over the free applicative functor `f`.
analyzeFreeAp :: forall f m a. Monoid m => (forall b. f b -> m) -> FreeAp f a -> m
analyzeFreeAp k = unwrap <<< foldFreeAp (Const <<< k)

instance functorFreeAp :: Functor (FreeAp f) where
  map k (Pure a) = Pure (k a)
  map k (Ap d) = Ap (k <$> d)

instance applyFreeAp :: Functor f => Apply (FreeAp f) where
  apply (Pure k) f = k <$> f
  apply (Ap d) e = runDay (\i f g -> Ap (day (\x (Tuple y a) -> i x y a) f (pure Tuple <*> g <*> e))) d

instance applicativeFreeAp :: Functor f => Applicative (FreeAp f) where
  pure = Pure
