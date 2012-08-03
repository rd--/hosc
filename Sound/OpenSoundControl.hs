-- | Composite of non-transport related modules.
--
-- Provides the 'Datum', 'Message', 'Bundle' and 'Packet' types and
-- the 'OSC' and 'Coding' type-classes.
--
-- The basic constructors are 'message' and 'bundle', the basic coding
-- functions are 'encodePacket' and 'decodePacket'.
--
-- > import Sound.OpenSoundControl
-- >
-- > let {o = bundle immediately [message "/g_free" [Int 0]]
-- >     ;e = encodeBundle o :: String}
-- > in decodeBundle e == o
module Sound.OpenSoundControl (module M) where

import Sound.OpenSoundControl.Class as M
import Sound.OpenSoundControl.Coding as M
import Sound.OpenSoundControl.Type as M
import Sound.OpenSoundControl.Time as M
import Sound.OpenSoundControl.Wait as M
