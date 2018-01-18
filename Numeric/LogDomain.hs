
-- | This module provides log-domain functionality. Ed Kmett provides, with
-- @log-domain@, a generic way to handle numbers in the log-domain, some which
-- is used under the hood here. We want some additional type safety and also
-- connect with the 'SemiRing' module.

module Numeric.LogDomain where

import Control.Monad.Except



-- | Instances for @LogDomain x@ should be for specific types.

class LogDomain x where
  -- | The data family to connect a type @x@ with the type @Ln x@ in the
  -- log-domain.
  data Ln x ∷ *
  -- | Transport a value in @x@ into the log-domain. @logdom@ should throw an
  -- exception if @log x@ is not valid.
  logdom ∷ (MonadError String m) ⇒ x → m (Ln x)
  -- | Unsafely transport x into the log-domain.
  unsafelogdom ∷ x → Ln x
  -- | Transport a value @Ln x@ back into the linear domain @x@.
  lindom ∷ Ln x → x

