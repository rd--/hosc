module Sound.OpenSoundControl.Time where

import Control.Monad
import qualified Data.Time as T

-- | UTC time is represented as a real number.
type UTC = Double

-- | NTP time is represented as an integer.
type NTP = Integer

-- | Convert a real-valued NTP timestamp to an NTP timestamp.
ntpr_ntp :: Double -> NTP
ntpr_ntp t = round (t * 2^(32::Int))

-- | Convert UTC timestamp to NTP timestamp.
utc_ntp :: UTC -> NTP
utc_ntp t = ntpr_ntp (t + secdif)
    where secdif = (70 * 365 + 17) * 24 * 60 * 60

-- | The time at 1970-01-01:00:00:00.
utc_base :: T.UTCTime
utc_base = T.UTCTime d s
    where d = T.fromGregorian 1970 1 1
          s = T.secondsToDiffTime 0

-- | Read current UTC timestamp.
utc :: IO UTC
utc = do t <- T.getCurrentTime
         return (realToFrac (T.diffUTCTime t utc_base))

-- | Read current NTP timestamp.
ntp :: IO NTP
ntp = liftM utc_ntp utc
