-- | This module defines a free applicative functor.
-- |
-- | The implementation of this module is based on Dave Menendez and
-- | Will Fancher's work.
-- |
-- | See [Free Applicative Functors in Haskell](https://www.eyrie.org/~zednenem/2013/05/27/freeapp) (Menendez 2013)
-- |
-- | See [The fraxl package](https://hackage.haskell.org/package/fraxl) (Fancher 2016)
module Control.Applicative.Free
  ( FreeAp
  , freeAp
  , unFreeAp
  , liftFreeAp
  , retractFreeAp
  , foldFreeAp
  , hoistFreeAp
  , analyzeFreeAp
  ) where

import Prelude hiding (ap)

import Data.ApList (ApList)
import Data.ApList as ApList
import Data.Const (Const(..))
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | The free applicative functor for a type constructor `f`.
newtype FreeAp f a = FreeAp (forall u y z. (forall x. (x -> y) -> ApList f x -> z) -> (u -> a -> y) -> ApList f u -> z)

freeAp :: forall f a. (forall u y z. (forall x. (x -> y) -> ApList f x -> z) -> (u -> a -> y) -> ApList f u -> z) -> FreeAp f a
freeAp = FreeAp

unFreeAp :: forall f a. FreeAp f a -> (forall u y z. (forall x. (x -> y) -> ApList f x -> z) -> (u -> a -> y) -> ApList f u -> z)
unFreeAp (FreeAp x) = x

-- | Lift a value described by the type constructor `f` into
-- | the free applicative functor.
liftFreeAp :: forall f a. f a -> FreeAp f a
liftFreeAp a = freeAp (\k f s -> k (\(Tuple a' s') -> f s' a') (ApList.cons a s))

-- | Run a free applicative functor using the applicative instance for
-- | the type constructor `f`.
retractFreeAp :: forall f a. Applicative f => FreeAp f a -> f a
retractFreeAp fa = unFreeAp fa (\f s -> f <$> ApList.reduce s) (\_ -> id) ApList.nil

-- | Run a free applicative functor with a natural transformation from
-- | the type constructor `f` to the applicative functor `g`.
foldFreeAp :: forall f g a. Applicative g => (f ~> g) -> FreeAp f a -> g a
foldFreeAp k = retractFreeAp <<< hoistFreeAp k

-- | Natural transformation from `FreeAp f a` to `FreeAp g a` given a
-- | natural transformation from `f` to `g`.
hoistFreeAp :: forall f g a. (f ~> g) -> FreeAp f a -> FreeAp g a
hoistFreeAp g x = freeAp (\k f s -> unFreeAp x (\f' s' -> ApList.rebase (ApList.hoist g s') k (\v u -> f v (f' u)) s) (const id) ApList.nil)

-- | Perform monoidal analysis over the free applicative functor `f`.
analyzeFreeAp :: forall f m a. Monoid m => (forall b. f b -> m) -> FreeAp f a -> m
analyzeFreeAp k = unwrap <<< foldFreeAp (Const <<< k)

instance functorFreeAp :: Functor (FreeAp f) where
  map g x = freeAp (\k f -> unFreeAp x k (\s -> f s <<< g))

instance applyFreeAp :: Apply (FreeAp f) where
  apply x y = freeAp (\k f -> unFreeAp y (unFreeAp x k) (\s a g -> f s (g a)))

instance applicativeFreeAp :: Applicative (FreeAp f) where
  pure a = freeAp (\k f -> k (_ `f` a))
