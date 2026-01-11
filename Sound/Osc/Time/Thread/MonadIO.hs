-- | MonadIO lifted forms of Sound.Osc.Time.Thread functions
module Sound.Osc.Time.Thread.MonadIO where

import Control.Monad.IO.Class {- base >= 4.9 -}

import qualified Sound.Osc.Time as Time {- hosc -}
import qualified Sound.Osc.Time.Thread as Time.Thread {- hosc -}

-- | 'liftIO' of 'currentTime'
time :: MonadIO m => m Time.NtpReal
time = liftIO Time.currentTime

-- | 'liftIO' of 'pauseThreadFor'
pauseThread :: (MonadIO m, RealFrac n) => n -> m ()
pauseThread = liftIO . Time.Thread.pauseThreadFor

-- | Alias for 'pauseThread'
wait :: MonadIO m => Double -> m ()
wait = pauseThread

-- | 'liftIO' of 'pauseThreadUntilTime'
pauseThreadUntil :: (MonadIO m, RealFrac n) => n -> m ()
pauseThreadUntil = liftIO . Time.Thread.pauseThreadUntilTime

-- | 'liftIO' of 'sleepThreadFor'
sleepThread :: (RealFrac n, MonadIO m) => n -> m ()
sleepThread = liftIO . Time.Thread.sleepThreadFor

-- | 'liftIO' of 'sleepThreadUntilTime'
sleepThreadUntil :: (RealFrac n, MonadIO m) => n -> m ()
sleepThreadUntil = liftIO . Time.Thread.sleepThreadUntilTime
