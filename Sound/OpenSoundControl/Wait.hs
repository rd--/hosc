module Sound.OpenSoundControl.Wait where

import System.Timeout

-- * Timeout

-- | Real valued variant of 'timeout'.
timeout_r :: Double -> IO a -> IO (Maybe a)
timeout_r t = timeout (floor (t * 1000000))

-- * Wait

-- | Repeat action until predicate /f/ is 'True' when applied to
-- result.
untilPredicate :: Monad m => (a -> Bool) -> m a -> m a
untilPredicate f act =
    let g p = if f p then rec else return p
        rec = act >>= g
    in rec

-- | Repeat action until /f/ does not give 'Nothing' when applied to
-- result.
untilMaybe :: Monad m => (a -> Maybe b) -> m a -> m b
untilMaybe f act =
    let g p = case f p of {Nothing -> rec;Just r -> return r}
        rec = act >>= g
    in rec
