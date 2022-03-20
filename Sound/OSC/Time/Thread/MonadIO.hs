-- | MonadIO lifted forms of Sound.OSC.Time.Thread functions
module Sound.OSC.Time.Thread.MonadIO where

import Control.Monad.IO.Class {- base >= 4.9 -}

import Sound.OSC.Time {- hosc -}
import Sound.OSC.Time.Thread {- hosc -}

time :: MonadIO m => m NtpReal
time = liftIO currentTime

pauseThread :: (MonadIO m,RealFrac n) => n -> m ()
pauseThread = liftIO . pauseThreadFor

wait :: MonadIO m => Double -> m ()
wait = pauseThread

sleepThread :: (RealFrac n, MonadIO m) => n -> m ()
sleepThread = liftIO . sleepThreadFor
