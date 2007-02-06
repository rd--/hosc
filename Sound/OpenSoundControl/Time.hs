module Sound.OpenSoundControl.Time (UTC, utc, NTP, ntp, utc_ntp) where

import System.Time (ClockTime(TOD), getClockTime)
import Control.Monad (liftM)

type UTC = Double
type NTP = Integer

-- | Convert UTC timestamp to NTP timestamp.
utc_ntp :: UTC -> NTP
utc_ntp t = secntp (t + secdif)
    where secntp i = round (i * 2^(32::Int))
          secdif = (70 * 365 + 17) * 24 * 60 * 60

-- | Read current UTC timestamp.
utc :: IO UTC
utc = do TOD s p <- getClockTime
         return (fromIntegral s + fromIntegral p / 1e12)

-- | Read current NTP timestamp.
ntp :: IO NTP
ntp = liftM utc_ntp utc
