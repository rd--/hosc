-- | Thread operations.
module Sound.Osc.Time.Thread where

import Control.Concurrent {- base -}
import Control.Monad {- base -}

import qualified Sound.Osc.Time as Time {- hosc -}

{- | The 'pauseThread' limit (in seconds).
  Values larger than this require a different thread delay mechanism, see 'sleepThread'.
  The value is the number of microseconds in @maxBound::Int@.
  This is only relevant for thirty-two bit systems, where the limit is thirty-six minutes.
  On sixty-four bit systems the limit is close to three-hundred thousand years.

>>> import Data.Int
>>> maxBound::Int32
2147483647

>>> 2 ^ 31 - 1
2147483647

>>> round (2147483647 / (1E6 * 60))
36

>>> maxBound::Int64
9223372036854775807

>>> 2 ^ 63 - 1
9223372036854775807

>>> round (9223372036854775807 / (1E6 * 60 * 60 * 24 * 365))
292471

> maxBound::Int
9223372036854775807

> round (pauseThreadLimit / 60)
153722867281
-}
pauseThreadLimit :: Fractional n => n
pauseThreadLimit = fromIntegral (maxBound :: Int) / 1e6

-- | Pause current thread for the indicated duration (in seconds), see 'pauseThreadLimit'.
pauseThreadFor :: RealFrac n => n -> IO ()
pauseThreadFor n = when (n > 0) (threadDelay (floor (n * 1e6)))

-- | Pause current thread until the given time, see 'pauseThreadLimit'.
pauseThreadUntilTime :: RealFrac n => n -> IO ()
pauseThreadUntilTime t = pauseThreadFor . (t -) . realToFrac =<< Time.currentTime

{- | Sleep current thread for the indicated duration (in seconds).
  Divides long sleeps into parts smaller than 'pauseThreadLimit'.
-}
sleepThreadFor :: RealFrac n => n -> IO ()
sleepThreadFor n =
  if n >= pauseThreadLimit
    then
      let n' = pauseThreadLimit - 1
      in pauseThreadFor n' >> sleepThreadFor (n - n')
    else pauseThreadFor n

{- | Sleep current thread until the given time.
  Divides long sleeps into parts smaller than 'pauseThreadLimit'.
-}
sleepThreadUntilTime :: RealFrac n => n -> IO ()
sleepThreadUntilTime t = sleepThreadFor . (t -) . realToFrac =<< Time.currentTime
