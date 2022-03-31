-- | MonadIO lifted forms of Sound.Osc.Time.Thread functions
module Sound.Osc.Time.Thread.MonadIO where

import Control.Monad.IO.Class {- base >= 4.9 -}

import Sound.Osc.Time {- hosc -}
import Sound.Osc.Time.Thread {- hosc -}

time :: MonadIO m => m NtpReal
time = liftIO currentTime

pauseThread :: (MonadIO m,RealFrac n) => n -> m ()
pauseThread = liftIO . pauseThreadFor

wait :: MonadIO m => Double -> m ()
wait = pauseThread

pauseThreadUntil :: (MonadIO m,RealFrac n) => n -> m ()
pauseThreadUntil = liftIO . pauseThreadUntilTime

sleepThread :: (RealFrac n, MonadIO m) => n -> m ()
sleepThread = liftIO . sleepThreadFor

sleepThreadUntil :: (RealFrac n, MonadIO m) => n -> m ()
sleepThreadUntil = liftIO . sleepThreadUntilTime
