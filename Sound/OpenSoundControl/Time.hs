-- | OSC related timing functions.  OSC timestamps are @NTP@ values,
-- <http://ntp.org/>.
module Sound.OpenSoundControl.Time where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T

-- * Temporal types

-- | Type for integer (binary) representation of @NTP@ time.
type NTPi = Word64

-- | @NTP@ time in real-valued (fractional) form.
type Time = Double

-- | @Unix/Posix@ epoch time in real-valued (fractional) form.
type UT = Double

-- * Time conversion

-- | Convert a real-valued NTP timestamp to an 'NTPi' timestamp.
ntpr_to_ntpi :: RealFrac n => n -> NTPi
ntpr_to_ntpi t = round (t * 2^(32::Int))

-- | Convert an 'NTPi' timestamp to a real-valued NTP timestamp.
ntpi_to_ntpr :: Fractional n => NTPi -> n
ntpi_to_ntpr t = fromIntegral t / 2^(32::Int)

-- | Difference (in seconds) between /NTP/ and /UT/ epochs.
--
-- > ntp_ut_epoch_diff / (24 * 60 * 60) == 25567
ntp_ut_epoch_diff :: Num n => n
ntp_ut_epoch_diff = (70 * 365 + 17) * 24 * 60 * 60

-- | Convert a 'UT' timestamp to an 'NTPi' timestamp.
ut_to_ntpi :: UT -> NTPi
ut_to_ntpi t = ntpr_to_ntpi (t + ntp_ut_epoch_diff)

-- | Convert @Unix/Posix@ to @NTP@.
ut_to_ntpr :: Num n => n -> n
ut_to_ntpr = (+) ntp_ut_epoch_diff

-- | Convert @NTP@ to @Unix/Posix@.
ntpr_to_ut :: Num n => n -> n
ntpr_to_ut = (+) (negate ntp_ut_epoch_diff)

-- | Convert 'NTPi' to @Unix/Posix@.
ntpi_to_ut :: NTPi -> UT
ntpi_to_ut = ntpr_to_ut . ntpi_to_ntpr

-- * Constants

-- | Constant indicating a bundle to be executed immediately.
--
-- > ntpr_to_ntpi immediately == 1
immediately :: Time
immediately = ntpi_to_ntpr 1

-- * 'Data.Time' inter-operation.

-- | The time at 1970-01-01:00:00:00.
ut_epoch :: T.UTCTime
ut_epoch =
    let d = T.fromGregorian 1970 1 1
        s = T.secondsToDiffTime 0
    in T.UTCTime d s

-- | Convert 'T.UTCTime' to @Unix/Posix@.
utc_to_ut :: Fractional n => T.UTCTime -> n
utc_to_ut t = realToFrac (T.diffUTCTime t ut_epoch)

-- * Clock operations

-- | Read current real-valued @NTP@ timestamp.
--
-- > do {ct <- fmap utc_to_ut T.getCurrentTime
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
--   'pauseThreadLimit'.
pauseThread :: (MonadIO m,Ord n,RealFrac n) => n -> m ()
pauseThread n = when (n > 0) (liftIO (threadDelay (floor (n * 1e6))))

-- | Type restricted 'pauseThread'.
wait :: MonadIO m => Double -> m ()
wait = pauseThread

-- | Pause current thread until the given 'Time', see
-- 'pauseThreadLimit'.
pauseThreadUntil :: MonadIO m => Time -> m ()
pauseThreadUntil t = pauseThread . (t -) =<< time

-- | Sleep current thread for the indicated duration (in seconds).
--   Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThread :: (RealFrac n, MonadIO m) => n -> m ()
sleepThread n =
    if n >= pauseThreadLimit
    then let n' = pauseThreadLimit - 1
         in pauseThread n >> sleepThread (n - n')
    else pauseThread n

-- | Sleep current thread until the given 'Time'.  Divides long sleeps
-- into parts smaller than 'pauseThreadLimit'.
sleepThreadUntil :: MonadIO m => Time -> m ()
sleepThreadUntil t = sleepThread . (t -) =<< time
