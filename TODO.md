
# TODOs

- consider if having the corresponding semiring explicitly is feasible, since
  we have to deal with values in dp algebras.

- figure if this all belongs in one package
- will be parent dependency for
  - rad-matrix based stuff
  - Bioinformatics / Biobase (natural sciences)
  - Linguistics (humanities)
  - Lib-SciBaseTypes

- The semiring to use is somewhat complicated, because there is more than one
  useful semiring. One strategy is to go for a useful encoding of all such
  possibilities and select the correct one via newtyping, or consider what
  makes for a useful default semiring.

# mathematics (algebraic structures)

- algebraic structure: semiring
  - (*), (+), 1, 0
  - mul, plus, mzero, pzero
  - Algebra.Structure(s).SemiRing
- algebraic structure: ring
  - additive inverse
  - Algebra.Structure(s).Ring
- algebraic structure: field
  - multiplicative inverse
  - Algebra.Structure(s).Field

- types:
  - Viterbi
  - Min/Max
  - Energy
  - Inside/Outside

# probability and statistics (Certainty, Statistics)

- probability
  - log
  - normalized / non-normalized
- likelihood
  - log
  - is proportional to probability
- marginal likelihood / model evidence / normalizing constant
- odds
  - log
  - discretized
- scores
  - defined via some odds or log-probs?
- counts
  - when counting for, say, a multinomial or binomial?

# Thermodynamics

- Gibbs free energy
  - metrology
  - haskell: units + units-def

# Statistical Mechanics

- branch of theoretical phystics using probability

- ensemble
  - class Ensemble
  - data family EnsembleType or type family EnsembleType
    - newtype instance EnsembleType DiscretizedLogOdds = Bla
    - type    instance EnsembleType DiscretizedLogOdds = LogProbability
    - this smells more like either a type family or a generic class that casts
      everything to LogProbability.
    - if we choose (log) probabilities, we end up with the definition of the
      grand canonical ensemble

