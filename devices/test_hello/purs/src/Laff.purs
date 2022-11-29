-- | Laff: Lightweight Aff
-- |
-- | Using Effect.Aff is causing out-of-memory errors on my ESP32 running Espruino.
-- | This module is an attempt at a lightweight monad that allows combining
-- | asynchronous effects via chaining callbacks, without all of Aff's "fiber" machinery.
-- |
-- | Unless, like me, you're trying to run Purescript on an extremely memory-poor platform
-- | and you don't really know what you're doing, you probably want to use Aff:
-- | https://pursuit.purescript.org/packages/purescript-aff/
-- |
-- | Vaugely: A value of type `Laff a` represents a computation that produces
-- | a result of type `a` and passes it to a continuation of type `a -> b`.
-- | For example, the internal representation of a function of type `a -> Laff a`
-- | that takes a value of type `a` and returns a `Laff a` value that calls its
-- | continuation after 5 seconds is:
-- | ```js
-- | function callIn5Secs(a) {
-- |   let laff = function(continuation) {
-- |     setTimeout(function(){ continuation(a) }, 5000);
-- |   }
-- |   return laff;
-- | }
-- | ```
-- | Monadic bind chains these callbacks together. For example,
-- | `callIn5Secs 99 >>= callIn5Secs` has the equivalent simplified representation:
-- | ```js
-- | function(continuation) {
-- |   setTimeout(function() {
-- |     setTimeout(function() {
-- |       continuation(99);
-- |     }, 5000);
-- |   }, 5000);
-- | }
-- | ```
-- |
-- | Example Usage:
-- | ```purs
-- | import Effect.Laff (Laff, delay, httpRequest)
-- | import Effect.Class (liftEffect)
-- |
-- | someLaff :: Laff String
-- | someLaff = do
-- |   ip <- httpRequest "ifconfig.me"
-- |   delay 500
-- |   liftEffect $ Console.log "hello"
-- |   delay 500
-- |   pure ip
-- | ```
module Effect.Laff where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Class (class MonadEffect)

foreign import data Laff :: Type -> Type

foreign import runLaff :: forall a. Laff a -> Effect Unit
foreign import delay :: Milliseconds -> Laff Unit
foreign import httpRequest :: String -> Laff String

-- The rest of this module comes direct from the Aff source code
foreign import _map :: forall a b. (a -> b) -> Laff a -> Laff b
foreign import _bind :: forall a b. Laff a -> (a -> Laff b) -> Laff b
foreign import _pure :: forall a. a -> Laff a

instance functorLaff :: Functor Laff where
  map = _map

instance applyLaff :: Apply Laff where
  apply = ap

instance applicativeLaff :: Applicative Laff where
  pure = _pure

instance bindLaff :: Bind Laff where
  bind = _bind

instance monadLaff :: Monad Laff

foreign import _liftEffect :: forall a. Effect a -> Laff a

instance monadEffectLaff :: MonadEffect Laff where
  liftEffect = _liftEffect

instance monadRecLaff :: MonadRec Laff where
  tailRecM k = go
    where
    go a = do
      res <- k a
      case res of
        Done r -> pure r
        Loop b -> go b
