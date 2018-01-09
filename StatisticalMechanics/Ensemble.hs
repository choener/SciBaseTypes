
-- | This module provides generic functionality to deal with ensembles in
-- statistical mechanics.

module StatisticalMechanics.Ensemble where



-- | The state probability functions provide conversion from some types @a@
-- into non-normalized probabilities. For "real" applications, using the
-- @logProbability@ function is preferred. This functions allows for easy
-- abstraction when types @a@ are given as fractions of some actual value (say:
-- deka-cal), or are discretized.
--
-- The returned values are not normalized, because we do not now the total
-- evidence @Z@ until integration over all states has happened -- which is not
-- feasible in a number of problems.
--
-- TODO replace @()@ with temperature and results with non-normalized @P@ or
-- @LogP@, depending.

class StateProbability a where
  -- | Given a temperature and a state "energy", return the corresponding
  -- non-normalized probability.
  stateProbability    ∷ () → a → ()
  stateLogProbability ∷ () → a → ()

