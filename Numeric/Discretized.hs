
-- | Discretized floating point numbers, where the scaling factor is kept
-- as two phantom types denoting the rational number used for scaling.

module Numeric.Discretized where

import Control.Applicative
import Data.Proxy
import Data.Ratio
import Debug.Trace
import GHC.Generics
import GHC.TypeLits
import GHC.Real (Ratio(..))



-- | A discretized value takes a floating point number @n@ and produces @n *
-- fromIntegral l / fromIntegral u@ where both @u@ and @l@ are given as
-- @TypeLits@. I.e. a scaling factor of @ (u / l) = (1 / 100)@ does all
-- calculations in subdivisions of 100.
--
-- The main use of a 'Discretized' value is to enable calculations with 'Int'
-- while somewhat pretending to use floating point values.
--
-- Be careful with certain operations like @(*)@ as they will easily cause the
-- numbers to arbitrarily wrong. @(+)@ and @(-)@ are fine, however.
--
-- NOTE Export and import of data is in the form of floating points, which can
-- lead to additional loss of precision if one is careless!
--
-- TODO fast 'Show' methods required!
--
-- TODO blaze stuff?
--
-- TODO We might want to discretize @LogDomain@ style values. This requires
-- some thought on in which direction to wrap. Maybe, we want to log-domain
-- Discretized values, which probably just works.

newtype Discretized (u ∷ Nat) (l ∷ Nat) = Discretized { getDiscretized ∷ Int }
  deriving (Eq,Ord,Generic,Show,Read)

instance (KnownNat u, KnownNat l) ⇒ Num (Discretized u l) where
  Discretized x + Discretized y = Discretized (x+y)
  Discretized x - Discretized y = Discretized (x-y)
  Discretized x * Discretized y =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (x*y*u) `div` l
  abs (Discretized x) = Discretized (abs x)
  signum (Discretized x) = Discretized $ signum x
  fromInteger = Discretized . fromInteger
  {-# Inline (+) #-}
  {-# Inline (-) #-}
  {-# Inline (*) #-}
  {-# Inline abs #-}
  {-# Inline signum #-}
  {-# Inline fromInteger #-}

instance Enum (Discretized u l) where
  toEnum = Discretized
  {-# Inline toEnum #-}
  fromEnum = getDiscretized
  {-# Inline fromEnum #-}

instance (Enum (Discretized u l), KnownNat u, KnownNat l) ⇒ Integral (Discretized u l) where

instance (KnownNat u, KnownNat l) ⇒ Fractional (Discretized u l) where
  Discretized x / Discretized y =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (x * l) `div` (y * u)
  {-# Inline (/) #-}
  recip (Discretized x) =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  error "need to find approximately ok transformation"
  {-# Inline recip #-}
  fromRational (a :% b) =
    let u = natVal @u Proxy
        l = natVal @l Proxy
    in  Discretized . fromInteger $ (a * l) `div` (b * u)

instance (KnownNat u, KnownNat l) ⇒ Real (Discretized u l) where
  toRational (Discretized d) =
    let u = natVal @u Proxy
        l = natVal @l Proxy
    in  (fromIntegral d * u) % l
  {-# Inline toRational #-}

-- | Discretizes any @Real a@ into the @Discretized@ value. This conversion
-- is /lossy/!

discretize ∷ forall a u l . (Real a, KnownNat u, KnownNat l) ⇒ a → Discretized u l
discretize a =
  let u = natVal @u Proxy
      l = natVal @l Proxy
      k = toRational a
  in  Discretized . fromIntegral $ numerator k * l `div` (denominator k * u)
{-# Inline discretize #-}

