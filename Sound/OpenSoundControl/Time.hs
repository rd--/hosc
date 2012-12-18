-- | OSC related timing functions.
--
-- OSC timestamps are @NTP@ values, <http://ntp.org/>.
-- 'T.getCurrentTime' gives @UTC@ values.  The 'Time' type is a union
-- of the different representations.
--
-- 'utcr' reads the current time as real valued @UTC@ and
-- 'pauseThread' suspends the current thread for a real valued number
-- of seconds.
module Sound.OpenSoundControl.Time where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T

-- * Temporal types

-- | Type for integer representation of NTP time.
type NTPi = Word64

-- | In /hosc/ time is represented in real-valued @NTP@ form.
type Time = Double

-- | Convert a real-valued NTP timestamp to an 'NTPi' timestamp.
ntpr_to_ntpi :: RealFrac n => n -> NTPi
ntpr_to_ntpi t = round (t * 2^(32::Int))

-- | Convert an 'NTPi' timestamp to a real-valued NTP timestamp.
ntpi_to_ntpr :: Fractional n => NTPi -> n
ntpi_to_ntpr t = fromIntegral t / 2^(32::Int)

-- | Unix time (real valued).
type UT = Double

-- | Difference (in seconds) between /NTP/ and /UT/ epochs.
ntp_ut_epoch_diff :: Num n => n
ntp_ut_epoch_diff = (70 * 365 + 17) * 24 * 60 * 60

-- | Convert a real-valued UTC timestamp to an 'NTPi' timestamp.
ut_to_ntpi :: UT -> NTPi
ut_to_ntpi t = ntpr_to_ntpi (t + ntp_ut_epoch_diff)

ut_to_ntpr :: Num n => n -> n
ut_to_ntpr = (+) ntp_ut_epoch_diff

-- | Convert a real-valued NTP timestamp to a real-valued UTC timestamp.
ntpr_to_ut :: Num n => n -> n
ntpr_to_ut t = t - ntp_ut_epoch_diff

-- | Convert an 'NTPi' timestamp to a real-valued UTC timestamp.
ntpi_to_ut :: NTPi -> UT
ntpi_to_ut = ntpr_to_ut . ntpi_to_ntpr

-- | The time at 1970-01-01:00:00:00.
ut_epoch :: T.UTCTime
ut_epoch =
    let d = T.fromGregorian 1970 1 1
        s = T.secondsToDiffTime 0
    in T.UTCTime d s

-- | Convert an 'T.UTCTime' timestamp to a real-valued UTC timestamp.
utc_utcr :: Fractional n => T.UTCTime -> n
utc_utcr t = realToFrac (T.diffUTCTime t ut_epoch)

-- | Constant indicating the bundle is to be executed immediately.
immediately :: Time
immediately = ntpi_to_ntpr 1

-- * Clock operations

-- | Read current real-valued @UTC@ timestamp.
--
-- > do {ct <- fmap utc_utcr T.getCurrentTime
-- >    ;pt <- fmap realToFrac T.getPOSIXTime
-- >    ;print (pt - ct,pt - ct < 1e-5)}
time :: MonadIO m => m Time
time = liftIO (fmap (ut_to_ntpr . realToFrac) T.getPOSIXTime)

-- * Thread operations.

-- | The 'pauseThread' limit (in seconds).  Values larger than this
-- require a different thread delay mechanism, see 'sleepThread'.  The
-- value is the number of microseconds in @maxBound::Int@.
pauseThreadLimit :: Fractional n => n
pauseThreadLimit = fromIntegral (maxBound::Int) / 1e6

-- | Pause current thread for the indicated duration (in seconds), see
--   'pauseThreadLimit'.  Note also that this function does not
--   attempt pauses less than @1e-4@.
pauseThread :: (MonadIO m,Ord n,RealFrac n) => n -> m ()
pauseThread n = when (n > 1e-4) (liftIO (threadDelay (floor (n * 1e6))))

-- | Pause current thread until the given real-valued @UTC@ time, see
-- 'pauseThreadLimit'.
pauseThreadUntil :: MonadIO m => Time -> m ()
pauseThreadUntil t = pauseThread . (t -) =<< time

-- | Sleep current thread for the indicated duration (in seconds).
--   Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThread :: MonadIO m => Double -> m ()
sleepThread n =
    if n >= pauseThreadLimit
    then let n' = pauseThreadLimit - 1
         in pauseThread n >> sleepThread (n - n')
    else pauseThread n

-- | Sleep current thread until the given real-valued @UTC@ time.
-- Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThreadUntil :: MonadIO m => Double -> m ()
sleepThreadUntil t = sleepThread . (t -) =<< time
