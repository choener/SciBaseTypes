
-- | A set with two binary operations, one for addition (@srplus@), one for
-- multiplication (@srmul@). Together with a neutral element for @srplus@,
-- named @srzero@, and one for @srmul@, named @srone@.

module Algebra.Structure.SemiRing where



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

