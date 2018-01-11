
-- | Provides newtypes for odds, log-odds, and discretized versions.

module Statistics.Odds where



-- | Encodes log-odds that have been rounded or clamped to integral numbers.
-- One advantage this provides is more efficient "maximum/minimum" calculations
-- compared to using @Double@s.

newtype DiscLogOdds = DiscLogOdds { getDiscLogOdds ∷ Int }


