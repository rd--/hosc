-- | Composite of "Sound.Osc.Core" and "Sound.Osc.Transport.Monad".
module Sound.Osc (module M) where

import Control.Monad.IO.Class as M (MonadIO, liftIO)
import Sound.Osc.Core as M
import Sound.Osc.Transport.Fd.Socket as M
import Sound.Osc.Transport.Monad as M
