
-- | Probability-related types.

module Statistics.Probability where

import Control.Lens
import Numeric.Log



data IsNormalized = Normalized | NotNormalized



-- * Probability in linear space

-- | @Prob@ wraps a @Double@ that encodes probabilities. If @Prob@ is tagged as
-- @Normalized@, the contained values are in the range @[0,...,1]@, otherwise
-- they are in the range @[0,...,∞]@.
--
-- TODO instances for serialization and further stuff

newtype Prob (n ∷ IsNormalized) x = Prob { getProb ∷ x }
  deriving (Eq,Ord,Show,Read)

deriving instance (Enum       x) ⇒ Enum       (Prob n x)
deriving instance (Num        x) ⇒ Num        (Prob n x)
deriving instance (Fractional x) ⇒ Fractional (Prob n x)
deriving instance (Floating   x) ⇒ Floating   (Prob n x)
deriving instance (Real       x) ⇒ Real       (Prob n x)
deriving instance (RealFrac   x) ⇒ RealFrac   (Prob n x)
deriving instance (RealFloat  x) ⇒ RealFloat  (Prob n x)

-- | Turns a value in a normalized probability. @error@ if the value is not in
-- the range @[0,...,1]@.

prob ∷ (Ord x, Num x, Show x) ⇒ x → Prob Normalized x
prob x
  | x >= 0 && x <= 1 = Prob x
  | otherwise        = error $ show x ++ " not in range of [0,...,1]"
{-# Inline prob #-}

-- | Simple wrapper around @Prob@ that fixes non-normalization.

prob' ∷ (Ord x, Num x, Show x) ⇒ x → Prob NotNormalized x
prob' = Prob
{-# Inline prob' #-}



-- * Probability in log space.

newtype LogProb (n ∷ IsNormalized) x = LogProb { getLogProb ∷ Log x }
  deriving (Eq,Ord)

deriving instance (Enum     (Log x)) ⇒ Enum     (LogProb n x)
deriving instance (Num      (Log x)) ⇒ Num      (LogProb n x)
--deriving instance (RealFrac (Log x)) ⇒ RealFrac (LogProb n x)
--deriving instance (RealFloat (Log x)) ⇒ RealFloat (LogProb n x)
--deriving instance (Floating x) ⇒ Floating (LogProb n x)
--deriving instance (Precise (Log x)) ⇒ Precise (LogProb n x)
--deriving instance (Fractional x) ⇒ Fractional (Prob n x)
--deriving instance (Real x) ⇒ Real (Prob n x)


instance (Show x) ⇒ Show (LogProb n x) where
  show (LogProb (Exp x)) = "LogProb " ++ show x



-- * Conversion between probability in linear and log-space.

-- | Turn probability into log-probability.

p2lp ∷ (Floating x) ⇒ Prob n x → LogProb n x
p2lp (Prob x) = LogProb . Exp $ log x
{-# Inline p2lp #-}

-- | Turn log-probability into probability.

lp2p ∷ (Floating x) ⇒ LogProb n x → Prob n x
lp2p (LogProb x) = Prob . exp $ ln x
{-# Inline lp2p #-}

-- | An isomorphism between @Prob@ and @LogProb@.

aslp ∷ (Floating x) ⇒ Iso' (Prob n x) (LogProb n x)
aslp = iso p2lp lp2p
{-# Inline aslp #-}

