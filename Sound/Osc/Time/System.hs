-- | System time
module Sound.Osc.Time.System where

import Data.Int {- base -}

import qualified Data.Time.Clock.System as Clock.System {- time >= 1.8 -}

import Sound.Osc.Time {- hosc -}

-- | Get the system time, epoch start of 1970 UTC, leap-seconds ignored.
--   getSystemTime is typically much faster than getCurrentTime, however it is not available in Hugs.
getSystemTimeAsNtpReal :: IO NtpReal
getSystemTimeAsNtpReal = do
  tm <- Clock.System.getSystemTime
  return (fromIntegral (Clock.System.systemSeconds tm) + (fromIntegral (Clock.System.systemNanoseconds tm) * 1.0e-9))

-- | System time with fractional part in microseconds (us) instead of nanoseconds (ns).
getSystemTimeInMicroseconds :: IO (Int64, Int)
getSystemTimeInMicroseconds = do
  tm <- Clock.System.getSystemTime
  return (Clock.System.systemSeconds tm, fromIntegral (Clock.System.systemNanoseconds tm) `div` 1000)
