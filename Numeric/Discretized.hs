
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

import Algebra.Structure.Semiring



-- | Some discretizations are of the type @ln 2 / 2@ (@PAM@ matrices in Blast
-- for example). Using this type, we can annotate as follows: @Discretized
-- (RTyLn 2 :% RTyId 2)@.

data RatioTy a = RTyExp a | RTyId a | RTyLn a | RTyPlus (RatioTy a) (RatioTy a) | RTyTimes (RatioTy a) (RatioTy a)

class RatioTyConstant a where
  ratioTyConstant ∷ Proxy a → Ratio Integer

instance (KnownNat k) ⇒ RatioTyConstant (RTyExp (k∷Nat)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let n = natVal @k Proxy in toRational (exp $ fromInteger n)

instance (KnownNat k) ⇒ RatioTyConstant (RTyId (k∷Nat)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let n = natVal @k Proxy in toRational n

instance (KnownNat k) ⇒ RatioTyConstant (RTyLn (k∷Nat)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = let n = natVal @k Proxy in toRational (log $ fromInteger n)

instance (RatioTyConstant a, RatioTyConstant b) ⇒ RatioTyConstant (RTyPlus (a∷RatioTy k) (b∷RatioTy k)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = ratioTyConstant @a Proxy + ratioTyConstant @b Proxy

instance (RatioTyConstant a, RatioTyConstant b) ⇒ RatioTyConstant (RTyTimes (a∷RatioTy k) (b∷RatioTy k)) where
  {-# Inline ratioTyConstant #-}
  ratioTyConstant Proxy = ratioTyConstant @a Proxy * ratioTyConstant @b Proxy

-- | A discretized value takes a floating point number @n@ and produces a
-- discretized value. The actual discretization formula is given on the type
-- level, freeing us from having to carry around some scaling function.
--
-- Typically, one might use types likes @100@, @(100 :% 1)@, or @(RTyLn 2 :%
-- RTyId 2)@.
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

newtype Discretized (b ∷ k) = Discretized { getDiscretized ∷ Int }
  deriving (Eq,Ord,Generic,Show,Read)

instance (KnownNat u, KnownNat l) ⇒ Num (Discretized ((u∷Nat) :% (l∷Nat))) where
  {-# Inline (+) #-}
  Discretized x + Discretized y = Discretized (x+y)
  {-# Inline (-) #-}
  Discretized x - Discretized y = Discretized (x-y)
  -- TODO it should be possible to generalize this over arbitrary value, or
  -- replace @KnownNat@ with the above @ratioTyConstant@.
  {-# Inline (*) #-}
  Discretized x * Discretized y =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (x*y*u) `div` l
  {-# Inline abs #-}
  abs (Discretized x) = Discretized (abs x)
  {-# Inline signum #-}
  signum (Discretized x) = Discretized $ signum x
  {-# Inline fromInteger #-}
  fromInteger = Discretized . fromInteger

instance Enum (Discretized b) where
  toEnum = Discretized
  {-# Inline toEnum #-}
  fromEnum = getDiscretized
  {-# Inline fromEnum #-}

-- instance (Enum (Discretized b), KnownNat u, KnownNat l) ⇒ Integral (Discretized u l) where

instance (KnownNat u, KnownNat l) ⇒ Fractional (Discretized ((u∷Nat) :% (l∷Nat))) where
  {-# Inline (/) #-}
  Discretized x / Discretized y =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  Discretized $ (x * l) `div` (y * u)
  {-# Inline recip #-}
  recip (Discretized x) =
    let u = fromInteger $ natVal @u Proxy
        l = fromInteger $ natVal @l Proxy
    in  error "need to find approximately ok transformation"
  {-# Inline fromRational #-}
  fromRational (a :% b) =
    let u = natVal @u Proxy
        l = natVal @l Proxy
    in  Discretized . fromInteger $ (a * l) `div` (b * u)

instance (KnownNat u, KnownNat l) ⇒ Real (Discretized ((u∷Nat) :% (l∷Nat))) where
  {-# Inline toRational #-}
  toRational (Discretized d) =
    let u = natVal @u Proxy
        l = natVal @l Proxy
    in  (fromIntegral d * u) % l

instance (Num (Discretized k)) ⇒ Semiring (Discretized k) where
  plus = (+)
  times = (*)
  zero = 0
  one = 1
  {-# Inline plus  #-}
  {-# Inline times #-}
  {-# Inline zero  #-}
  {-# Inline one   #-}

-- | Discretizes any @Real a@ into the @Discretized@ value. This conversion
-- is /lossy/ and uses a type-level rational of @u :% l@!

discretizeRatio ∷ forall a u l . (Real a, KnownNat u, KnownNat l) ⇒ a → Discretized ((u∷Nat) :% (l∷Nat))
{-# Inline discretizeRatio #-}
discretizeRatio a =
  let u = natVal @u Proxy
      l = natVal @l Proxy
      k = toRational a
  in  Discretized . fromIntegral $ numerator k * l `div` (denominator k * u)

