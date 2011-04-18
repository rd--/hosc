-- | Temporal representations and clock operations (read current time
--   and pause thread).
module Sound.OpenSoundControl.Time where

import Control.Concurrent
import Control.Monad
import Data.Word (Word64)
import qualified Data.Time as T

type NTPi = Word64

-- | Time is represented in either UTC or NTP form.
data Time = UTCr Double | NTPr Double | NTPi NTPi
            deriving (Read, Show)

instance Eq Time where
    a == b = as_ntpi a == as_ntpi b
    a /= b = as_ntpi a /= as_ntpi b

-- | Coerce to NTPi form.
as_ntpi :: Time -> NTPi
as_ntpi (UTCr t) = utcr_ntpi t
as_ntpi (NTPr t) = ntpr_ntpi t
as_ntpi (NTPi t) = t

-- | Coerce to UTCr form.
as_utcr :: Time -> Double
as_utcr (UTCr t) = t
as_utcr (NTPr t) = ntpr_utcr t
as_utcr (NTPi t) = ntpi_utcr t

-- | Times can be ordered, avoid coercion if not required.
instance Ord Time where
    compare (UTCr p) (UTCr q) = compare p q
    compare (NTPr p) (NTPr q) = compare p q
    compare (NTPi p) (NTPi q) = compare p q
    compare p q = compare (as_ntpi p) (as_ntpi q)

-- | Convert a real-valued NTP timestamp to an NTP timestamp.
ntpr_ntpi :: Double -> NTPi
ntpr_ntpi t = round (t * 2^(32::Int))

-- | Convert an NTP timestamp to a real-valued NTP timestamp.
ntpi_ntpr :: NTPi -> Double
ntpi_ntpr t = fromIntegral t / 2^(32::Int)

-- | Convert UTC timestamp to NTP timestamp.
utcr_ntpi :: Double -> NTPi
utcr_ntpi t = ntpr_ntpi (t + secdif)
    where secdif = (70 * 365 + 17) * 24 * 60 * 60

-- | Convert NTP timestamp to UTC timestamp.
ntpr_utcr :: Double -> Double
ntpr_utcr t = t - secdif
    where secdif = (70 * 365 + 17) * 24 * 60 * 60

-- | Convert NTP timestamp to UTC timestamp.
ntpi_utcr :: NTPi -> Double
ntpi_utcr = ntpr_utcr . ntpi_ntpr

-- | The time at 1970-01-01:00:00:00.
utc_base :: T.UTCTime
utc_base = T.UTCTime d s
    where d = T.fromGregorian 1970 1 1
          s = T.secondsToDiffTime 0

-- | Read current UTCr timestamp.
utcr :: IO Double
utcr = do t <- T.getCurrentTime
          return (realToFrac (T.diffUTCTime t utc_base))

-- | Read current NTP timestamp.
ntpi :: IO NTPi
ntpi = liftM utcr_ntpi utcr

-- | Pause current thread for the indicated duration, given in seconds.
pauseThread :: Double -> IO ()
pauseThread n = when (n > 1e-4) (threadDelay (floor (n * 1e6)))

-- | Pause current thread until the given utcr time.
pauseThreadUntil :: Double -> IO ()
pauseThreadUntil t = pauseThread . (t -) =<< utcr

-- | Execute the bundle immediately.
immediately :: Time
immediately = NTPi 1
