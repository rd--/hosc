-- | Composite of "Sound.OSC.Core" and "Sound.OSC.Transport.Monad".
module Sound.OSC (module M) where

import Control.Monad.IO.Class as M (MonadIO,liftIO)
import Sound.OSC.Core as M
--import Sound.OSC.Transport.FD.UDP as M
--import Sound.OSC.Transport.FD.TCP as M
import Sound.OSC.Transport.Monad as M
