
-- | Probability-related types.
--
-- TODO instances for serialization and further stuff
-- TODO vector instances

module Statistics.Probability where

import Control.Lens
import Numeric.Log
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)

import Algebra.Structure.SemiRing
import Numeric.LogDomain
import Numeric.Limits



data IsNormalized = Normalized | NotNormalized



-- * Probability in linear space

-- | @Prob@ wraps a @Double@ that encodes probabilities. If @Prob@ is tagged as
-- @Normalized@, the contained values are in the range @[0,...,1]@, otherwise
-- they are in the range @[0,...,∞]@.

newtype Prob (n ∷ IsNormalized) x = Prob { getProb ∷ x }
  deriving (Eq,Ord,Show,Read)

derivingUnbox "Prob"
  [t| forall n x. Unbox x ⇒ Prob n x → x |]  [| getProb |]  [| Prob |]

deriving instance (Enum       x) ⇒ Enum       (Prob n x)
deriving instance (Num        x) ⇒ Num        (Prob n x)
deriving instance (Fractional x) ⇒ Fractional (Prob n x)
deriving instance (Floating   x) ⇒ Floating   (Prob n x)
deriving instance (Real       x) ⇒ Real       (Prob n x)
deriving instance (RealFrac   x) ⇒ RealFrac   (Prob n x)
deriving instance (RealFloat  x) ⇒ RealFloat  (Prob n x)

instance (Num r) ⇒ SemiRing (Prob n r) where
  srplus = (+)
  srmul  = (*)
  srzero = 0
  srone  = 1

-- | Turns a value into a normalized probability. @error@ if the value is not
-- in the range @[0,...,1]@.

prob ∷ (Ord x, Num x, Show x) ⇒ x → Prob Normalized x
prob x
  | x >= 0 && x <= 1 = Prob x
  | otherwise        = error $ show x ++ " not in range of [0,...,1]"
{-# Inline prob #-}

-- | Simple wrapper around @Prob@ that fixes non-normalization.

prob' ∷ (Ord x, Num x, Show x) ⇒ x → Prob NotNormalized x
prob' = Prob
{-# Inline prob' #-}



-- * Probability in log space. A number of operations internally cast to @Log@
-- from @log-domain@, but the values themselves are *not* newtype-wrapped @Log
-- x@ values. This is because we want to be *explicit* that these are
-- log-probabilities.
--
-- @Log@ numbers in cases like @fromIntegral 1 :: Log Double@ are treated as
-- not being in the log-domain, hence @fromIntegral performs a @log@
-- operations.

newtype LogProb (n ∷ IsNormalized) x = LogProb { getLogProb ∷ x }
  deriving (Eq,Ord,Show)

derivingUnbox "LogProb"
  [t| forall n x. Unbox x ⇒ LogProb n x → x |]  [| getLogProb |]  [| LogProb |]

instance (Precise x, RealFloat x) ⇒ Num (LogProb n x) where
  (+) = withLog2 (+)
  (*) = withLog2 (*)
  abs = withLog1 abs
  signum = withLog1 signum
  fromInteger = LogProb . fromInteger
  negate = withLog1 negate
  (-) = withLog2 (-)

instance (Num d, Fractional d) ⇒ NumericLimits (LogProb n d) where
  minFinite = LogProb 0
  maxFinite = LogProb (1/0)

withLog1 ∷ (Log x → Log y) → LogProb n x → LogProb n y
withLog1 op (LogProb x) = LogProb . ln $ op (Exp x)
{-# Inline withLog1 #-}

withLog2 ∷ (Log x → Log y → Log z) → LogProb n x → LogProb n y → LogProb n z
withLog2 op (LogProb x) (LogProb y) = LogProb . ln $ op (Exp x) (Exp y)
{-# Inline withLog2 #-}


-- * Conversion between probability in linear and log-space.

-- | Turn probability into log-probability.

p2lp ∷ (Floating x) ⇒ Prob n x → LogProb n x
p2lp (Prob x) = LogProb $ log x
{-# Inline p2lp #-}

-- | Turn log-probability into probability.

lp2p ∷ (Floating x) ⇒ LogProb n x → Prob n x
lp2p (LogProb x) = Prob $ exp x
{-# Inline lp2p #-}

-- | An isomorphism between @Prob@ and @LogProb@.

aslp ∷ (Floating x) ⇒ Iso' (Prob n x) (LogProb n x)
aslp = iso p2lp lp2p
{-# Inline aslp #-}

