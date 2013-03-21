-- | Composite of non-transport related modules.
--
-- Provides the 'Datum', 'Message', 'Bundle' and 'Packet' types and
-- the 'OSC' and 'Coding' type-classes.
--
-- The basic constructors are 'message' and 'bundle', the basic coding
-- functions are 'encodePacket' and 'decodePacket'.
--
-- > import Sound.OSC.Core
-- >
-- > let {o = bundle immediately [message "/g_free" [Int 0]]
-- >     ;e = encodeBundle o :: String}
-- > in decodeBundle e == o
module Sound.OSC.Core (module M) where

import Sound.OSC.Class as M
import Sound.OSC.Coding as M
import Sound.OSC.Datum as M
import Sound.OSC.Normalise as M
import Sound.OSC.Time as M
import Sound.OSC.Type as M
import Sound.OSC.Wait as M
