module Sound.OpenSoundControl.Time where

import Control.Concurrent
import Control.Monad
import qualified Data.Time as T

-- | Time is represented in either UTC or NTP form.
data Time = UTCr Double | NTPr Double | NTPi Integer
            deriving (Eq, Show)

-- | Coerce to NTPi form.
as_ntpi :: Time -> Integer
as_ntpi (UTCr t) = utcr_ntpi t
as_ntpi (NTPr t) = ntpr_ntpi t
as_ntpi (NTPi t) = t

-- | Times can be ordered, avoid coercion if not required.
instance Ord Time where
    compare (UTCr p) (UTCr q) = compare p q
    compare (NTPr p) (NTPr q) = compare p q
    compare (NTPi p) (NTPi q) = compare p q
    compare p q = compare (as_ntpi p) (as_ntpi q)

-- | Convert a real-valued NTP timestamp to an NTP timestamp.
ntpr_ntpi :: Double -> Integer
ntpr_ntpi t = round (t * 2^(32::Int))

-- | Convert UTC timestamp to NTP timestamp.
utcr_ntpi :: Double -> Integer
utcr_ntpi t = ntpr_ntpi (t + secdif)
    where secdif = (70 * 365 + 17) * 24 * 60 * 60

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
ntpi :: IO Integer
ntpi = liftM utcr_ntpi utcr

-- | Pause current thread for the indicated duration, given in seconds.
pauseThread :: Double -> IO ()
pauseThread n = when (n > 1e-4) (threadDelay (floor (n * 1e6)))

-- | Pause current thread until the given utcr time.
pauseThreadUntil :: Double -> IO ()
pauseThreadUntil t = pauseThread . (t -) =<< utcr
