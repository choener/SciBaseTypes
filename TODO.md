
# TODOs

- figure if this all belong in one package
- will be parent dependency for
  - rad-matrix based stuff
  - Bioinformatics / Biobase (natural sciences)
  - Linguistics (humanities)
  - Lib-SciBaseTypes

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

