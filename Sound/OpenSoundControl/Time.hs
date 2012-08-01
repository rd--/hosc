-- | Temporal representations and clock operations (read current time
--   and pause thread).
module Sound.OpenSoundControl.Time where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import qualified Data.Time as T

-- * Temporal types

-- | Type for integer representation of NTP time.
type NTPi = Word64

-- | Time is represented in either @UTC@ or @NTP@ form.  The @NTP@ form may
--   be either integral or real.
data Time = UTCr Double | NTPr Double | NTPi NTPi
            deriving (Read, Show)

instance Eq Time where
    a == b = as_ntpi a == as_ntpi b
    a /= b = as_ntpi a /= as_ntpi b

-- | Coerce 'Time' to integral @NTP@ form.
as_ntpi :: Time -> NTPi
as_ntpi x =
    case x of
      UTCr t -> utcr_ntpi t
      NTPr t -> ntpr_ntpi t
      NTPi t -> t

-- | Coerce 'Time' to real-valued @UTC@ form.
as_utcr :: Time -> Double
as_utcr x =
    case x of
      UTCr t -> t
      NTPr t -> ntpr_utcr t
      NTPi t -> ntpi_utcr t

-- | Times can be ordered, avoid coercion if not required.
instance Ord Time where
    compare p q =
        case (p,q) of
          (UTCr p',UTCr q') -> compare p' q'
          (NTPr p',NTPr q') -> compare p' q'
          (NTPi p',NTPi q') -> compare p' q'
          _ -> compare (as_ntpi p) (as_ntpi q)

-- | Convert a real-valued NTP timestamp to an 'NTPi' timestamp.
ntpr_ntpi :: Double -> NTPi
ntpr_ntpi t = round (t * 2^(32::Int))

-- | Convert an 'NTPi' timestamp to a real-valued NTP timestamp.
ntpi_ntpr :: NTPi -> Double
ntpi_ntpr t = fromIntegral t / 2^(32::Int)

-- | Convert a real-valued UTC timestamp to an 'NTPi' timestamp.
utcr_ntpi :: Double -> NTPi
utcr_ntpi t =
    let secdif = (70 * 365 + 17) * 24 * 60 * 60
    in ntpr_ntpi (t + secdif)

-- | Convert a real-valued NTP timestamp to a real-valued UTC timestamp.
ntpr_utcr :: Double -> Double
ntpr_utcr t =
    let secdif = (70 * 365 + 17) * 24 * 60 * 60
    in t - secdif

-- | Convert an 'NTPi' timestamp to a real-valued UTC timestamp.
ntpi_utcr :: NTPi -> Double
ntpi_utcr = ntpr_utcr . ntpi_ntpr

-- | The time at 1970-01-01:00:00:00.
utc_base :: T.UTCTime
utc_base =
    let d = T.fromGregorian 1970 1 1
        s = T.secondsToDiffTime 0
    in T.UTCTime d s

utc_utcr :: T.UTCTime -> Double
utc_utcr t = realToFrac (T.diffUTCTime t utc_base)

-- | Constant indicating the bundle is to be executed immediately.
immediately :: Time
immediately = NTPi 1

-- * Clock operations

-- | Read current real-valued @UTC@ timestamp.
utcr :: MonadIO m => m Double
utcr = liftIO (fmap utc_utcr T.getCurrentTime)

-- | Read current 'NTPi' timestamp.
ntpi ::  MonadIO m => m NTPi
ntpi = liftM utcr_ntpi utcr

-- | The 'pauseThread' limit (in seconds).  Values larger than this
-- require a different thread delay mechanism, see 'sleepThread'.  The
-- value is the number of microseconds in @maxBound::Int@.
pauseThreadLimit :: Double
pauseThreadLimit = fromIntegral (maxBound::Int) / 1e6

-- | Pause current thread for the indicated duration (in seconds), see
--   'pauseThreadLimit'.  Note also that this function does not
--   attempt pauses less than @1e-4@.
pauseThread :: MonadIO m => Double -> m ()
pauseThread n = when (n > 1e-4) (liftIO (threadDelay (floor (n * 1e6))))

-- | Pause current thread until the given real-valued @UTC@ time, see
-- 'pauseThreadLimit'.
pauseThreadUntil :: MonadIO m => Double -> m ()
pauseThreadUntil t = pauseThread . (t -) =<< utcr

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
sleepThreadUntil t = sleepThread . (t -) =<< utcr
