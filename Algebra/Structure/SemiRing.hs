
-- | A set with two binary operations, one for addition (@srplus@), one for
-- multiplication (@srmul@). Together with a neutral element for @srplus@,
-- named @srzero@, and one for @srmul@, named @srone@.

module Algebra.Structure.SemiRing where

import GHC.Generics



-- * The 'SemiRing' type class.

-- | The semiring operations and neutral elements.

class SemiRing a where
  srplus  ∷ a → a → a
  srmul   ∷ a → a → a
  srzero  ∷ a
  srone   ∷ a

-- | Unicode variant of @srplus@.

(⊕) ∷ SemiRing a ⇒ a → a → a
(⊕) = srplus

-- | Unicode variant of @srmul@.

(⊗) ∷ SemiRing a ⇒ a → a → a
(⊗) = srmul



-- * Newtype wrappers for 'SemiRing' that make the semiring to use explicit.
-- This is important, because several types, say Prob(ability) have multiple
-- useful semiring instances.
--
-- 'Data.Monoid' in @base@ provides a number of newtype wrappers (@Sum@,
-- @Product@, etc) for monoids, which have one binary operation and identity.
-- There is, obviously, overlap with the structures constructed here.

newtype Viterbi x = Viterbi { getViterbi ∷ x }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

-- | Shall we have generic instances, or specific ones like @SemiRing (Viterbi
-- Prob)@?

instance (Ord x, Num x) ⇒ SemiRing (Viterbi x) where
  srplus (Viterbi x) (Viterbi y) = Viterbi $ max x y
  srmul  (Viterbi x) (Viterbi y) = Viterbi $ x * y
  srzero = Viterbi 0
  srone  = Viterbi 1

