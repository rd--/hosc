-- | System time
module Sound.Osc.Time.System where

import Data.Int {- base -}
import Data.Word {- base -}

import qualified Data.Time.Clock.System as Clock.System {- time >= 1.8 -}

import qualified Sound.Osc.Time as Time {- hosc -}

{- | Get the system time, epoch start of 1970 UTC, leap-seconds ignored.
  getSystemTime is typically much faster than getCurrentTime, however it is not available in Hugs.
-}
getSystemTimeAsNtpReal :: IO Time.NtpReal
getSystemTimeAsNtpReal = do
  tm <- Clock.System.getSystemTime
  return (fromIntegral (Clock.System.systemSeconds tm) + (fromIntegral (Clock.System.systemNanoseconds tm) * 1.0e-9))

-- | System time with fractional part in microseconds (us) instead of nanoseconds (ns).
getSystemTimeInMicroseconds :: IO (Int64, Word32)
getSystemTimeInMicroseconds = do
  tm <- Clock.System.getSystemTime
  let sec = Clock.System.systemSeconds tm
      usec = Clock.System.systemNanoseconds tm `div` 1000
  return (sec, usec)
