module Test.Control.Applicative.Free.Validation
  ( User
  , runForm
  ) where

import Prelude (class Show, show, (<<<), (==), (<$>), (<*>), (<>))

import Control.Applicative.Free (FreeAp, foldFreeAp, liftFreeAp)
import Effect.Exception.Unsafe (unsafeThrow)

import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (maybe)

type Validator a = String -> Either String a

newtype Field a = Field { name :: String, validator :: Validator a }

type Form = FreeAp Field

field :: forall a. String -> Validator a -> Form a
field n v = liftFreeAp (Field { name: n, validator: v })

nes :: String -> Form String
nes n = field n (\v -> if v == "" then Left (n <> ": Invalid NES") else Right v)

int :: String -> Form Int
int n = field n (maybe (Left (n <> ": Invalid Int")) Right <<< fromString)

newtype User = User { firstName :: String, lastName :: String, age :: Int }

form :: Form User
form =
  User <$> ({ firstName: _
            , lastName: _
            , age: _
            } <$> nes "First name"
              <*> nes "Last name"
              <*> int "Age")

runForm :: String -> String -> String -> Either String User
runForm first last age =
  foldFreeAp (\(Field x) -> validate x.name x.validator) form
  where
  validate :: forall a. String -> Validator a -> Either String a
  validate n v =
    case n of
         "First name" -> v first
         "Last name" -> v last
         "Age" -> v age
         _ -> unsafeThrow ("Unexpected field: " <> n)

instance showUser :: Show User where
  show (User m) = m.firstName <> " " <> m.lastName <> " " <> show m.age
