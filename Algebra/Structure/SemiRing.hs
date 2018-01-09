
-- | A set with two binary operations, one for addition (@splus@), one for
-- multiplication (@smul@). Together with a neutral element for @splus@, named
-- @szero@, and one for @smul@, named @sone@.

module Algebra.Structure.SemiRing where



-- | The semiring operations and neutral elements.

class SemiRing a where
  splus ∷ a → a → a
  smul  ∷ a → a → a
  szero ∷ a
  sone  ∷ a

-- | Unicode variant of @splus@.

(⊕) ∷ SemiRing a ⇒ a → a → a
(⊕) = splus

-- | Unicode variant of @smul@.

(⊗) ∷ SemiRing a ⇒ a → a → a
(⊗) = smul

