-- | Composite of "Sound.OpenSoundControl" and
-- "Sound.OpenSoundControl.Transport.Monad".
module Sound.OSC (module M) where

import Sound.OpenSoundControl as M
import Sound.OpenSoundControl.Transport.FD.UDP as M
import Sound.OpenSoundControl.Transport.FD.TCP as M
import Sound.OpenSoundControl.Transport.Monad as M
