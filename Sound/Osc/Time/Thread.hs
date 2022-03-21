-- | Thread operations.
module Sound.Osc.Time.Thread where

import Control.Concurrent {- base -}
import Control.Monad {- base -}

import Sound.Osc.Time {- hosc -}

-- | The 'pauseThread' limit (in seconds).
--   Values larger than this require a different thread delay mechanism, see 'sleepThread'.
--   The value is the number of microseconds in @maxBound::Int@.
pauseThreadLimit :: Fractional n => n
pauseThreadLimit = fromIntegral (maxBound::Int) / 1e6

-- | Pause current thread for the indicated duration (in seconds), see 'pauseThreadLimit'.
pauseThreadFor :: RealFrac n => n -> IO ()
pauseThreadFor n = when (n > 0) (threadDelay (floor (n * 1e6)))

-- | Pause current thread until the given time, see 'pauseThreadLimit'.
pauseThreadUntil :: NtpReal -> IO ()
pauseThreadUntil t = pauseThreadFor . (t -) =<< currentTime

-- | Sleep current thread for the indicated duration (in seconds).
--   Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThreadFor :: RealFrac n => n -> IO ()
sleepThreadFor n =
    if n >= pauseThreadLimit
    then let n' = pauseThreadLimit - 1
         in pauseThreadFor n >> sleepThreadFor (n - n')
    else pauseThreadFor n

-- | Sleep current thread until the given time.
--   Divides long sleeps into parts smaller than 'pauseThreadLimit'.
sleepThreadUntil :: NtpReal -> IO ()
sleepThreadUntil t = sleepThreadFor . (t -) =<< currentTime
