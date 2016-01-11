module Control.Applicative.Free
  ( FreeAp()
  , NaturalTransformation()
  , liftFreeAp
  , retractFreeAp
  , foldFreeAp
  , hoistFreeAp
  , analyzeFreeAp
  ) where

import Prelude (Applicative, Apply, Functor, Unit(), (<<<), apply, flip, id, map, pure, unit)

import Data.Const (Const(..), getConst)
import Data.Exists (Exists(), mkExists, runExists)
import Data.Monoid (Monoid)

-- | The free applicative functor for a type constructor `f`.
data FreeAp f a = Pure a | Ap (Exists (ApF f a))

data ApF f a i = ApF (Unit -> f i) (Unit -> FreeAp f (i -> a))

type NaturalTransformation f g = forall a. f a -> g a

ap :: forall f a i. (Unit -> f i) -> (Unit -> FreeAp f (i -> a)) -> FreeAp f a
ap v k = Ap (mkExists (ApF v k))

-- | Lift a value described by the type constructor `f` into
-- | the free applicative functor.
liftFreeAp :: forall f a. f a -> FreeAp f a
liftFreeAp a = ap (\_ -> a) (\_ -> Pure id)

-- | Run a free applicative functor using the applicative instance for
-- | the type constructor `f`.
retractFreeAp :: forall f a. (Applicative f) => FreeAp f a -> f a
retractFreeAp (Pure a) = pure a
retractFreeAp (Ap x) = runExists (\(ApF v k') -> apply (retractFreeAp (k' unit)) (v unit)) x

-- | Run a free applicative functor with a natural transformation from
-- | the type constructor `f` to the applicative functor `g`.
foldFreeAp :: forall f g a. (Applicative g) => NaturalTransformation f g -> FreeAp f a -> g a
foldFreeAp k (Pure a) = pure a
foldFreeAp k (Ap x) = runExists (\(ApF v k') -> apply (map (flip id) (k (v unit))) (foldFreeAp k (k' unit))) x

-- | Natural transformation from `FreeAp f a` to `FreeAp g a` given a
-- | natural transformation from `f` to `g`.
hoistFreeAp :: forall f g a. NaturalTransformation f g -> FreeAp f a -> FreeAp g a
hoistFreeAp k (Pure a) = Pure a
hoistFreeAp k (Ap x) = runExists (\(ApF v k') -> ap (\_ -> k (v unit)) (\_ -> hoistFreeAp k (k' unit))) x

-- | Perform monoidal analysis over the free applicative functor `f`.
analyzeFreeAp :: forall f m a. (Monoid m) => (forall b. f b -> m) -> FreeAp f a -> m
analyzeFreeAp k = getConst <<< foldFreeAp (Const <<< k)

instance functorFreeAp :: Functor (FreeAp f) where
  map k (Pure a) = Pure (k a)
  map k (Ap x) = runExists (\(ApF v k') -> ap v (\_ -> map ((<<<) k) (k' unit))) x

instance applyFreeAp :: Apply (FreeAp f) where
  apply (Pure k) f = map k f
  apply (Ap x) f = runExists (\(ApF v k') -> ap v (\_ -> apply (map flip (k' unit)) f)) x

instance applicativeFreeAp :: Applicative (FreeAp f) where
  pure = Pure
