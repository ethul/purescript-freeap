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
import Data.Either (Either(..))
import Data.List (List(..))
import Data.List.NonEmpty as NEL
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

-- | The free applicative functor for a type constructor `f`.
data FreeAp f a
  = Pure a
  | Lift (f a)
  | Ap (FreeAp f (Val -> a)) (FreeAp f Val)

data Val

-- | Lift a value described by the type constructor `f` into
-- | the free applicative functor.
liftFreeAp :: forall f a. f a -> FreeAp f a
liftFreeAp = Lift

type ApFunc g = { func :: g (Val -> Val), count :: Int }
type FuncStack g = List (ApFunc g)
type ValStack f = NEL.NonEmptyList (FreeAp f Val)
type Stack f g = Tuple (FuncStack g) (ValStack f)

-- | Run a free applicative functor with a natural transformation from
-- | the type constructor `f` to the applicative functor `g`.
foldFreeAp :: forall f g a. Applicative g => (f ~> g) -> FreeAp f a -> g a
foldFreeAp nat z =
  unsafeToG $ go $ Tuple Nil (NEL.singleton $ unsafeToFVal z)
  where
  unsafeToG :: g Val -> g a
  unsafeToG = unsafeCoerce

  unsafeToFVal :: forall f' a'. FreeAp f' a' -> FreeAp f' Val
  unsafeToFVal = unsafeCoerce

  go :: Stack f g -> g Val
  go (Tuple fStack (NEL.NonEmptyList (val :| vals))) =
    case val of
      Pure a -> case goApply fStack vals (pure a) of
        Left x -> x
        Right s -> go s
      Lift a -> case goApply fStack vals (nat a) of
        Left x -> x
        Right s -> go s
      Ap l r ->
        let nextVals = NEL.NonEmptyList (r :| vals)
        in go $ goLeft fStack nextVals nat l 1

goApply
  :: forall f g
  . Applicative g
  => FuncStack g
  -> List (FreeAp f Val)
  -> g Val
  -> Either (g Val) (Stack f g)
goApply fStack vals gVal =
  case fStack of
    Nil -> Left gVal
    Cons f fs ->
      let gRes = f.func <*> gVal
      in if f.count == 1 then
        case fs of
          Nil ->
            -- here vals must be empty
            Left gRes
          _ -> goApply fs vals gRes
        else
          case vals of
            Nil -> Left gRes
            Cons val vals' ->
              Right $ Tuple
                (Cons { func: unsafeToGFunc gRes, count: f.count - 1 } fs)
                (NEL.NonEmptyList (val :| vals'))
  where
  unsafeToGFunc :: g Val -> g (Val -> Val)
  unsafeToGFunc = unsafeCoerce

goLeft
  :: forall f g
  . Applicative g
  => FuncStack g
  -> ValStack f
  -> (f ~> g)
  -> FreeAp f (Val -> Val)
  -> Int
  -> Stack f g
goLeft fStack valStack nat func count = case func of
  Pure a -> Tuple (Cons { func: pure a, count } fStack) valStack
  Lift a -> Tuple (Cons { func: nat a, count } fStack) valStack
  Ap l r -> goLeft fStack (NEL.cons r valStack) nat (unsafeToFunc l) (count + 1)
  where
  unsafeToFunc :: FreeAp f (Val -> Val -> Val) -> FreeAp f (Val -> Val)
  unsafeToFunc = unsafeCoerce

-- | Run a free applicative functor using the applicative instance for
-- | the type constructor `f`.
retractFreeAp :: forall f a. Applicative f => FreeAp f a -> f a
retractFreeAp = foldFreeAp identity

-- | Natural transformation from `FreeAp f a` to `FreeAp g a` given a
-- | natural transformation from `f` to `g`.
hoistFreeAp :: forall f g a. (f ~> g) -> FreeAp f a -> FreeAp g a
hoistFreeAp f = foldFreeAp (f >>> liftFreeAp)

-- | Perform monoidal analysis over the free applicative functor `f`.
analyzeFreeAp :: forall f m a. Monoid m => (forall b. f b -> m) -> FreeAp f a -> m
analyzeFreeAp k = unwrap <<< foldFreeAp (Const <<< k)

mkAp :: forall f a b. FreeAp f (b -> a) -> FreeAp f b -> FreeAp f a
mkAp fba fb = Ap (coerceFunc fba) (coerceValue fb)
  where
  coerceFunc :: FreeAp f (b -> a) -> FreeAp f (Val -> a)
  coerceFunc = unsafeCoerce

  coerceValue :: FreeAp f b -> FreeAp f Val
  coerceValue = unsafeCoerce

instance functorFreeAp :: Functor (FreeAp f) where
  map f x = mkAp (Pure f) x

instance applyFreeAp :: Apply (FreeAp f) where
  apply fba fb = mkAp fba fb

instance applicativeFreeAp :: Applicative (FreeAp f) where
  pure = Pure
