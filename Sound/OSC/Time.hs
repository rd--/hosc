-- | Osc related timing functions.
--   Osc timestamps are 64-bit @Ntp@ values, <http://ntp.org/>.
module Sound.OSC.Time where

import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Data.Int {- base -}
import Data.Word {- base -}

import qualified Control.Monad.IO.Class {- base >= 4.9 -}

import qualified Data.Time as Time {- time -}
import qualified Data.Time.Clock as Clock {- time -}
import qualified Data.Time.Clock.POSIX as Clock.Posix {- time -}
import qualified Data.Time.Clock.System as Clock.System {- time -}

import Sound.OSC.Coding.Convert {- hosc -}

-- * Temporal types

-- | Type for binary (integeral) representation of a 64-bit @NTP@ timestamp (ie. @ntpi@).
--   The NTP epoch is January 1, 1900.
--   NTPv4 also includes a 128-bit format, which is not used by OSC.
type NTP64 = Word64

-- | @NTP@ time in real-valued (fractional) form (ie. @ntpr@).
--   This is the primary form of timestamp used by hosc.
type Time = Double

-- | Constant indicating a bundle to be executed immediately.
--   It has the NTP64 representation of @1@.
immediately :: Time
immediately = 1 / 2^(32::Int)

-- | @Unix/Posix@ time in real-valued (fractional) form.
--   The Unix/Posix epoch is January 1, 1970.
type UT = Double

-- * Time conversion

{- | Convert a real-valued NTP timestamp to an 'NTPi' timestamp.

> ntpr_to_ntpi 0 == 0
> ntpr_to_ntpi immediately == 1
> fmap ntpr_to_ntpi time
-}
ntpr_to_ntpi :: Time -> NTP64
ntpr_to_ntpi t = round (t * (2 ^ (32::Int)))

-- | Convert an 'NTPi' timestamp to a real-valued NTP timestamp.
ntpi_to_ntpr :: NTP64 -> Time
ntpi_to_ntpr t = word64_to_double t / 2^(32::Int)

{- | Difference (in seconds) between /NTP/ and /UT/ epochs.

> ntp_ut_epoch_diff / (24 * 60 * 60) == 25567
> 25567 `div` 365 == 70
-}
ntp_ut_epoch_diff :: Num n => n
ntp_ut_epoch_diff = (70 * 365 + 17) * 24 * 60 * 60

-- | Convert a 'UT' timestamp to an 'NTPi' timestamp.
ut_to_ntpi :: UT -> NTP64
ut_to_ntpi t = ntpr_to_ntpi (t + ntp_ut_epoch_diff)

-- | Convert @Unix/Posix@ to @NTP@.
ut_to_ntpr :: Num n => n -> n
ut_to_ntpr = (+) ntp_ut_epoch_diff

-- | Convert @NTP@ to @Unix/Posix@.
ntpr_to_ut :: Num n => n -> n
ntpr_to_ut = (+) (negate ntp_ut_epoch_diff)

-- | Convert 'NTPi' to @Unix/Posix@.
ntpi_to_ut :: NTP64 -> UT
ntpi_to_ut = ntpr_to_ut . ntpi_to_ntpr

-- | Convert 'Time' to 'Clock.Posix.POSIXTime'.
ntpr_to_posixtime :: Time -> Clock.Posix.POSIXTime
ntpr_to_posixtime = realToFrac . ntpr_to_ut

-- | Convert 'Clock.Posix.POSIXTime' to 'Time'.
posixtime_to_ntpr :: Clock.Posix.POSIXTime -> Time
posixtime_to_ntpr = ut_to_ntpr . realToFrac

-- * 'Data.Time' inter-operation.

-- | The time at 1970-01-01:00:00:00.
ut_epoch :: Time.UTCTime
ut_epoch =
    let d = Time.fromGregorian 1970 1 1
        s = Time.secondsToDiffTime 0
    in Time.UTCTime d s

-- | Convert 'Time.UTCTime' to @Unix/Posix@.
utc_to_ut :: Fractional n => Time.UTCTime -> n
utc_to_ut t = realToFrac (Time.diffUTCTime t ut_epoch)

-- * Clock operations

-- | Get the system time, epoch start of 1970 UTC, leap-seconds ignored.
--   getSystemTime is typically much faster than getCurrentTime, however it is not available in Hugs.
getSystemTimeAsDouble :: IO Double
getSystemTimeAsDouble = do
  tm <- Clock.System.getSystemTime
  return (fromIntegral (Clock.System.systemSeconds tm) + (fromIntegral (Clock.System.systemNanoseconds tm) * 1.0e-9))

-- | System time with fractional part in microseconds (us) instead of nanoseconds (ns).
getSystemTimeInMicroseconds :: IO (Int64,Int)
getSystemTimeInMicroseconds = do
  tm <- Clock.System.getSystemTime
  return (Clock.System.systemSeconds tm,fromIntegral (Clock.System.systemNanoseconds tm) `div` 1000)

-- | utc_to_ut of Clock.getCurrentTime.
getCurrentTimeAsDouble :: IO Time
getCurrentTimeAsDouble = fmap utc_to_ut Clock.getCurrentTime

{- | Read current real-valued @NTP@ timestamp.

> get_ct = fmap utc_to_ut Clock.getCurrentTime
> get_pt = fmap realToFrac Clock.Posix.getPOSIXTime
> (ct,pt) <- get_ct >>= \t0 -> get_pt >>= \t1 -> return (t0,t1)
> print (pt - ct,pt - ct < 1e-5)

-}
time :: Control.Monad.IO.Class.MonadIO m => m Time
time = Control.Monad.IO.Class.liftIO (fmap posixtime_to_ntpr Clock.Posix.getPOSIXTime)

-- * Thread operations.

-- | The 'pauseThread' limit (in seconds).
--   Values larger than this require a different thread delay mechanism, see 'sleepThread'.
--   The value is the number of microseconds in @maxBound::Int@.
pauseThreadLimit :: Fractional n => n
pauseThreadLimit = fromIntegral (maxBound::Int) / 1e6

-- | Pause current thread for the indicated duration (in seconds), see 'pauseThreadLimit'.
pauseThread :: (Control.Monad.IO.Class.MonadIO m,RealFrac n) => n -> m ()
pauseThread n = when (n > 0) (Control.Monad.IO.Class.liftIO (threadDelay (floor (n * 1e6))))

-- | Type restricted 'pauseThread'.
wait :: Control.Monad.IO.Class.MonadIO m => Double -> m ()
wait = pauseThread

-- | Pause current thread until the given 'Time', see 'pauseThreadLimit'.
pauseThreadUntil :: Control.Monad.IO.Class.MonadIO m => Time -> m ()
pauseThreadUntil t = pauseThread . (t -) =<< time

-- | Sleep current thread for the indicated duration (in seconds).
--   Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThread :: (RealFrac n, Control.Monad.IO.Class.MonadIO m) => n -> m ()
sleepThread n =
    if n >= pauseThreadLimit
    then let n' = pauseThreadLimit - 1
         in pauseThread n >> sleepThread (n - n')
    else pauseThread n

-- | Sleep current thread until the given 'Time'.
--   Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThreadUntil :: Control.Monad.IO.Class.MonadIO m => Time -> m ()
sleepThreadUntil t = sleepThread . (t -) =<< time

-- * Pretty printing

-- | Detailed 37-character ISO 8601 format, including fractional seconds and '+0000' suffix.
iso_8601_fmt :: String
iso_8601_fmt = "%Y-%m-%dT%H:%M:%S,%q+0000"

{- | Parse time according to 'iso_8601_fmt'

> iso_8601_to_utctime "2015-11-26T00:29:37,145875000000+0000"
-}
iso_8601_to_utctime :: String -> Maybe Time.UTCTime
iso_8601_to_utctime = Time.parseTimeM True Time.defaultTimeLocale iso_8601_fmt

-- | UTC time in 'iso_8601_fmt'.
--
-- > tm <- fmap (utctime_to_iso_8601 . Clock.Posix.posixSecondsToUTCTime) Clock.Posix.getPOSIXTime
-- > (length tm,sum [4+1+2+1+2,1,2+1+2+1+2,1,12,1,4],sum [10,1,8,1,12,1,4]) == (37,37,37)
utctime_to_iso_8601 :: Time.UTCTime -> String
utctime_to_iso_8601 = Time.formatTime Time.defaultTimeLocale iso_8601_fmt

{- | ISO 8601 of 'Time'.

> tm <- fmap ntpr_to_iso_8601 time
> import System.Process {- process -}
> rawSystem "date" ["-d",tm]

> t = 15708783354150518784
> s = "2015-11-26T00:22:19,366058349609+0000"
> ntpr_to_iso_8601 (ntpi_to_ntpr t) == s
-}
ntpr_to_iso_8601 :: Time -> String
ntpr_to_iso_8601 = utctime_to_iso_8601 . Clock.Posix.posixSecondsToUTCTime . ntpr_to_posixtime

{- | 'Time' of ISO 8601.

> t = 15708783354150518784
> s = "2015-11-26T00:22:19,366058349609+0000"
> fmap ntpr_to_ntpi (iso_8601_to_ntpr s) == Just t
-}
iso_8601_to_ntpr :: String -> Maybe Time
iso_8601_to_ntpr = fmap (posixtime_to_ntpr . Clock.Posix.utcTimeToPOSIXSeconds) . iso_8601_to_utctime

{- | Alias for 'ntpr_to_iso_8601'.

> time_pp immediately == "1900-01-01T00:00:00,000000000000+0000"
> fmap time_pp time
-}
time_pp :: Time -> String
time_pp = ntpr_to_iso_8601
