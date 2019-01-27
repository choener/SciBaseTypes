
-- | A set with two binary operations, one for addition (@srplus@), one for
-- multiplication (@srmul@). Together with a neutral element for @srplus@,
-- named @srzero@, and one for @srmul@, named @srone@.

module Algebra.Structure.SemiRing where

import Control.DeepSeq (NFData(..))
import Data.Coerce
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)
import GHC.Generics
import Unsafe.Coerce

import Numeric.Limits



-- * The 'SemiRing' type class.

-- | The semiring operations and neutral elements.

class SemiRing a where
  srplus  ∷ a → a → a
  srmul   ∷ a → a → a
  srzero  ∷ a
  srone   ∷ a

-- | Unicode variant of @srplus@.

infixl 6 ⊕
infixl 6 `srplus`
(⊕) ∷ SemiRing a ⇒ a → a → a
(⊕) = srplus
{-# Inline (⊕) #-}

-- | Unicode variant of @srmul@.

infixl 7 ⊗
infixl 7 `srmul`
(⊗) ∷ SemiRing a ⇒ a → a → a
(⊗) = srmul
{-# Inline (⊗) #-}



-- * Newtype wrappers for 'SemiRing' that make the semiring to use explicit.
-- This is important, because several types, say Prob(ability) have multiple
-- useful semiring instances.
--
-- 'Data.Monoid' in @base@ provides a number of newtype wrappers (@Sum@,
-- @Product@, etc) for monoids, which have one binary operation and identity.
-- There is, obviously, overlap with the structures constructed here.

-- | The Viterbi SemiRing. It maximizes over the product.

newtype Viterbi x = Viterbi { getViterbi ∷ x }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

derivingUnbox "Viterbi"
  [t| forall x . Unbox x ⇒ Viterbi x → x |]  [| getViterbi |]  [| Viterbi |]

instance NFData x ⇒ NFData (Viterbi x) where
  rnf (Viterbi x) = rnf x
  {-# Inline rnf #-}

-- |
--
-- TODO Shall we have generic instances, or specific ones like @SemiRing
-- (Viterbi Prob)@?
--
-- TODO Consider either a constraint @ProbLike x@ or the above.

instance (Ord x, Num x) ⇒ SemiRing (Viterbi x) where
  srplus (Viterbi x) (Viterbi y) = Viterbi $ max x y
  srmul  (Viterbi x) (Viterbi y) = Viterbi $ x * y
  srzero = Viterbi 0
  srone  = Viterbi 1
  {-# Inline srplus #-}
  {-# Inline srmul  #-}
  {-# Inline srzero #-}
  {-# Inline srone  #-}

-- | The tropical MinPlus SemiRing. It minimizes over the sum.

newtype MinPlus x = MinPlus { getMinPlus ∷ x }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

derivingUnbox "MinPlus"
  [t| forall x . Unbox x ⇒ MinPlus x → x |]  [| getMinPlus |]  [| MinPlus |]

instance NFData x ⇒ NFData (MinPlus x) where
  rnf (MinPlus x) = rnf x
  {-# Inline rnf #-}

-- |
--
-- TODO Shall we have generic instances, or specific ones like @SemiRing
-- (Viterbi Prob)@?
--
-- TODO Consider either a constraint @ProbLike x@ or the above.

instance (Ord x, Num x, NumericLimits x) ⇒ SemiRing (MinPlus x) where
  srplus (MinPlus x) (MinPlus y) = MinPlus $ min x y
  srmul  (MinPlus x) (MinPlus y) = MinPlus $ x + y
  srzero = MinPlus maxFinite
  srone  = 0
  {-# Inline srplus #-}
  {-# Inline srmul  #-}
  {-# Inline srzero #-}
  {-# Inline srone  #-}



-- | The tropical MaxPlus SemiRing. It maximizes over the sum.

newtype MaxPlus x = MaxPlus { getMaxPlus ∷ x }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

derivingUnbox "MaxPlus"
  [t| forall x . Unbox x ⇒ MaxPlus x → x |]  [| getMaxPlus |]  [| MaxPlus |]

instance NFData x ⇒ NFData (MaxPlus x) where
  rnf (MaxPlus x) = rnf x
  {-# Inline rnf #-}

instance NumericLimits x ⇒ NumericLimits (MaxPlus x) where
  minFinite = MaxPlus minFinite
  maxFinite = MaxPlus maxFinite

-- |
--
-- TODO Shall we have generic instances, or specific ones like @SemiRing
-- (Viterbi Prob)@?
--
-- TODO Consider either a constraint @ProbLike x@ or the above.

instance (Ord x, Num x, NumericLimits x) ⇒ SemiRing (MaxPlus x) where
  srplus (MaxPlus x) (MaxPlus y) = MaxPlus $ max x y
  srmul  (MaxPlus x) (MaxPlus y) = MaxPlus $ x + y
  srzero = MaxPlus minFinite
  srone  = 0
  {-# Inline srplus #-}
  {-# Inline srmul  #-}
  {-# Inline srzero #-}
  {-# Inline srone  #-}



-- * Generic semiring structure encoding.

-- | The generic semiring, defined over two 'Semigroup' and 'Monoid'
-- constructions.
--
-- It can be used like this:
-- @
-- srzero ∷ GSemiRing Min Sum Int  == maxBound
-- srone  ∷ GSemiRing Min Sum Int  == 0
-- @
--
-- It is generally useful to still provide explicit instances, since @Min@
-- requires a @Bounded@ instance.

newtype GSemiRing (zeroMonoid ∷ * → *) (oneMonoid ∷ * → *) (x ∷ *) = GSemiRing { getSemiRing ∷ x }
  deriving (Eq, Ord, Read, Show, Generic)

instance
  forall zeroMonoid oneMonoid x
  . ( Semigroup (zeroMonoid x)
    , Monoid    (zeroMonoid x)
    , Semigroup ( oneMonoid x)
    , Monoid    ( oneMonoid x)
    )
  ⇒ SemiRing (GSemiRing zeroMonoid oneMonoid x) where
  srplus (GSemiRing x) (GSemiRing y) =
    let x' ∷ zeroMonoid x = unsafeCoerce x
        y' ∷ zeroMonoid x = unsafeCoerce y
    in  unsafeCoerce $ x' <> y'
  srmul (GSemiRing x) (GSemiRing y) =
    let x' ∷ oneMonoid x = unsafeCoerce x
        y' ∷ oneMonoid x = unsafeCoerce y
    in  unsafeCoerce $ x' <> y'
  srzero = unsafeCoerce (mempty ∷ zeroMonoid x)
  srone  = unsafeCoerce (mempty ∷  oneMonoid x)
  {-# Inline srplus #-}
  {-# Inline srmul  #-}
  {-# Inline srzero #-}
  {-# Inline srone  #-}

-- ** Variants of 'Semigroup' structures, that use @NumericLimits@ instead of
-- @Bounded@.



-- * Probability Semiring
--
-- | The probability semiring is defined on non-negative real numbers and uses
-- the usual @*@ and @+@ operations. For many "real-world" applications, it
-- should wrap @log-domain@ numbers for increased numerical stability.
--
-- This semiring has some real-world problems, in that we need to assume that
-- all values are @[0..1]@. Hence we also provide non-normalized probabilities.

newtype Probability x = Probability { getProbability ∷ x }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

derivingUnbox "Probability"
  [t| forall x . Unbox x ⇒ Probability x → x |]  [| getProbability |]  [| Probability |]

instance NFData x ⇒ NFData (Probability x) where
  rnf (Probability x) = rnf x
  {-# Inline rnf #-}

instance (Num x, NumericLimits x) ⇒ NumericLimits (Probability x) where
  minFinite = Probability 0
  maxFinite = Probability 1

-- |

instance (Ord x, Num x, NumericLimits x) ⇒ SemiRing (Probability x) where
  srplus (Probability x) (Probability y) = Probability $ x + y
  srmul  (Probability x) (Probability y) = Probability $ x * y
  srzero = Probability 0
  srone  = Probability 1
  {-# Inline srplus #-}
  {-# Inline srmul  #-}
  {-# Inline srzero #-}
  {-# Inline srone  #-}



-- * Log-semiring.
--
-- | 

newtype LogSR x = LogSR { getLogSR ∷ x }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

derivingUnbox "LogSR"
  [t| forall x . Unbox x ⇒ LogSR x → x |]  [| getLogSR |]  [| LogSR |]

instance NFData x ⇒ NFData (LogSR x) where
  rnf (LogSR x) = rnf x
  {-# Inline rnf #-}

instance (Num x, NumericLimits x) ⇒ NumericLimits (LogSR x) where
  minFinite = LogSR 0
  maxFinite = LogSR maxFinite

-- |

instance (Ord x, Num x, Fractional x, NumericLimits x) ⇒ SemiRing (LogSR x) where
  srplus (LogSR x) (LogSR y)
    | x <= y    = undefined
    | otherwise = undefined
  srmul  (LogSR x) (LogSR y) = LogSR $ x + y
  srzero = LogSR (negate 1 / 0)
  srone  = LogSR 0
  {-# Inline srplus #-}
  {-# Inline srmul  #-}
  {-# Inline srzero #-}
  {-# Inline srone  #-}

