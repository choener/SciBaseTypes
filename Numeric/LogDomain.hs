
-- | This module provides log-domain functionality. Ed Kmett provides, with
-- @log-domain@, a generic way to handle numbers in the log-domain, some which
-- is used under the hood here. We want some additional type safety and also
-- connect with the 'SemiRing' module.

module Numeric.LogDomain where

import Control.Monad.Except
import Numeric.Log as NL
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Fusion.Util as SM
import Debug.Trace



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
-- but with the potential for numeric problems. In pricinple, the numeric error
-- of this function should be better than individual binary function
-- application and worse than an optimized @sum@ function.
--
-- Needs to be written in direct style, as otherwise any constructors (to tell
-- us if we collected two elements already) remain.

logsumexpS
  ∷ (Monad m, Ord a, Num a, Floating a)
  ⇒ SM.Stream m a → m a
{-# Inline logsumexpS #-}
logsumexpS (SM.Stream step s0) = lseLoop0 SM.SPEC s0
  where
    lseLoop0 !_ s = step s >>= \case
      SM.Done        → return 0
      SM.Skip    s0' → lseLoop0 SM.SPEC s0'
      SM.Yield x s1  → lseLoop1 SM.SPEC x s1
    lseLoop1 !_ x s = step s >>= \case
      SM.Done        → return x
      SM.Skip    s1' → lseLoop1 SM.SPEC x s1'
      SM.Yield y sA  → let !m = max x y in lseLoopAcc SM.SPEC m (exp (x-m) + exp (y-m)) sA
    lseLoopAcc !_ !m !acc s = step s >>= \case
      SM.Done        → return $ m + log acc
      SM.Skip    sA' → lseLoopAcc SM.SPEC m acc sA'
      SM.Yield z sA' → lseLoopAcc SM.SPEC m (acc + exp (z-m)) sA'

