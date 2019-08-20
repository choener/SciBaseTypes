
-- | This module provides log-domain functionality. Ed Kmett provides, with
-- @log-domain@, a generic way to handle numbers in the log-domain, some which
-- is used under the hood here. We want some additional type safety and also
-- connect with the 'SemiRing' module.

module Numeric.LogDomain where

import Control.Monad.Except
import Numeric.Log
import qualified Data.Vector.Fusion.Stream.Monadic as SM



-- | Instances for @LogDomain x@ should be for specific types.

class LogDomain x where
  -- | The type family to connect a type @x@ with the type @Ln x@ in the
  -- log-domain.
  type Ln x ∷ *
  -- | Transport a value in @x@ into the log-domain. @logdom@ should throw an
  -- exception if @log x@ is not valid.
  logdom ∷ (MonadError String m) ⇒ x → m (Ln x)
  -- | Unsafely transport x into the log-domain.
  unsafelogdom ∷ x → Ln x
  -- | Transport a value @Ln x@ back into the linear domain @x@.
  lindom ∷ Ln x → x



instance LogDomain Double where
  type Ln Double = Log Double
  {-# Inline logdom #-}
  logdom x
    | x < 0     = throwError "log of negative number"
    | otherwise = return $ unsafelogdom x
  {-# Inline unsafelogdom #-}
  unsafelogdom = Exp . log
  {-# Inline lindom #-}
  lindom = exp . ln


-- | @log-sum-exp@ for streams, without incurring examining the stream twice,
-- but with the potential for numeric problems.

lseStream ∷ (a → a → a) → a → SM.Stream m a → a
{-# Inline lseStream #-}
lseStream mplus mzero = finalize . SM.foldl' go (LSEzero mzero) where
  go (LSEzero !z   ) x = LSEone x
--  go (LSEone  !o   ) x = LSEacc (o x
--  go (LSEtwo  !o !t) x = LSEacc (max o t) 

data LSE a
  = LSEzero
  | LSEone !a
--  | LSEtwo !a !a
  | LESacc !a !a

