module Control.Lazy where

import Prelude

-- | The `Lazy` class represents types which allow evaluation of values
-- | to be _deferred_.
-- |
-- | Usually, this means that a type contains a function arrow which can
-- | be used to delay evaluation.
class Lazy l where
  defer :: (Unit -> l) -> l

instance lazyFn :: Lazy (a -> b) where
  defer f = \x -> f unit x

deferMonad :: forall m a. Monad m => (Unit -> m a) -> m a
deferMonad f = pure unit >>= (defer \_ -> f)

-- | `fix` defines a value as the fixed point of a function.
-- |
-- | The `Lazy` instance allows us to generate the result lazily.
fix :: forall l. Lazy l => (l -> l) -> l
fix f = defer (\_ -> f (fix f))
