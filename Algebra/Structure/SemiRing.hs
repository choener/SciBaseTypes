
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
  srzero = MinPlus minFinite
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

