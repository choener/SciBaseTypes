
-- | Provides newtypes for odds, log-odds, and discretized versions.

module Statistics.Odds where

import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Vector.Unboxed.Deriving
import GHC.Generics (Generic)

import Numeric.Limits



-- | Odds.

newtype Odds = Odds { getOdds ∷ Double }
  deriving (Generic,Eq,Ord,Show,Read,Num)

-- | Encodes log-odds that have been rounded or clamped to integral numbers.
-- One advantage this provides is more efficient "maximum/minimum" calculations
-- compared to using @Double@s.
--
-- Note that these are "explicit" log-odds. Each numeric operation uses the
-- underlying operation on @Int@.

newtype DiscLogOdds = DiscLogOdds { getDiscLogOdds ∷ Int }
  deriving (Generic,Eq,Ord,Show,Read,Num)

derivingUnbox "DiscretizedLogOdds"
  [t| DiscLogOdds → Int |]  [| getDiscLogOdds |]  [| DiscLogOdds |]

instance Binary    DiscLogOdds
instance Serialize DiscLogOdds
instance FromJSON  DiscLogOdds
instance ToJSON    DiscLogOdds
instance Hashable  DiscLogOdds

instance NFData DiscLogOdds where
  rnf (DiscLogOdds k) = rnf k
  {-# Inline rnf #-}

instance NumericLimits DiscLogOdds where
  minFinite = DiscLogOdds minFinite
  {-# Inline minFinite #-}
  maxFinite = DiscLogOdds maxFinite
  {-# Inline maxFinite #-}

