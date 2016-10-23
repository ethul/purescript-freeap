## Module Control.Applicative.Free

#### `FreeAp`

``` purescript
data FreeAp f a
```

The free applicative functor for a type constructor `f`.

##### Instances
``` purescript
Functor (FreeAp f)
Apply (FreeAp f)
Applicative (FreeAp f)
```

#### `liftFreeAp`

``` purescript
liftFreeAp :: forall f a. f a -> FreeAp f a
```

Lift a value described by the type constructor `f` into
the free applicative functor.

#### `retractFreeAp`

``` purescript
retractFreeAp :: forall f a. Applicative f => FreeAp f a -> f a
```

Run a free applicative functor using the applicative instance for
the type constructor `f`.

#### `foldFreeAp`

``` purescript
foldFreeAp :: forall f g a. Applicative g => (f ~> g) -> FreeAp f a -> g a
```

Run a free applicative functor with a natural transformation from
the type constructor `f` to the applicative functor `g`.

#### `hoistFreeAp`

``` purescript
hoistFreeAp :: forall f g a. (f ~> g) -> FreeAp f a -> FreeAp g a
```

Natural transformation from `FreeAp f a` to `FreeAp g a` given a
natural transformation from `f` to `g`.

#### `analyzeFreeAp`

``` purescript
analyzeFreeAp :: forall f m a. Monoid m => (forall b. f b -> m) -> FreeAp f a -> m
```

Perform monoidal analysis over the free applicative functor `f`.


